{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Error.Util         as Err
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Network.Wreq               as Wreq
import qualified Options.Applicative        as Opt
import qualified Options.Applicative.Text   as Opt
import qualified Web.Giphy                  as Giphy

import           Control.Applicative        (optional, (<**>), (<|>))
import           Control.Lens               (Getting (), preview)
import           Control.Lens.At            (at)
import           Control.Lens.Cons          (_head)
import           Control.Lens.Operators
import           Control.Lens.Prism         (_Left, _Right)
import           Control.Monad              (join, unless)
import           Data.Monoid                (First (), (<>))
import           Data.Version               (Version (), showVersion)
import           Paths_givegif              (version)
import           System.Environment         (getProgName)
import           System.IO                  (stderr)

import           Console

data Options = Options
  { optNoPreview :: Bool
  , optMode      :: SearchMode
  }

data SearchMode = OptSearch T.Text | OptTranslate T.Text | OptRandom (Maybe T.Text)

options :: Opt.Parser Options
options =
  Options <$> Opt.switch ( Opt.long "no-preview"
                        <> Opt.short 'n'
                        <> Opt.help "Don't render an inline image preview." )
          <*> ( ( OptSearch <$> Opt.textOption ( Opt.long "search"
                                          <> Opt.short 's'
                                          <> Opt.help "Use search to find a matching GIF." ) )
          <|> ( OptTranslate <$> Opt.textOption ( Opt.long "translate"
                                           <> Opt.short 't'
                                           <> Opt.help "Use translate to find a matching GIF." ) )
          <|> ( OptRandom <$> optional ( Opt.textArgument ( Opt.metavar "RANDOM_TAG" ) ) ) )

cliParser :: String -> Version -> Opt.ParserInfo Options
cliParser progName ver =
  Opt.info ( Opt.helper <*> options <**> versionInfo )
    ( Opt.fullDesc
   <> Opt.progDesc "Find GIFs on the command line."
   <> Opt.header progName )
  where
    versionInfo = Opt.infoOption ( unwords [progName, showVersion ver] )
      ( Opt.short 'V'
     <> Opt.long "version"
     <> Opt.hidden
     <> Opt.help "Show version information" )

apiKey :: Giphy.Key
apiKey = Giphy.Key "dc6zaTOxFJmzC"

taggedPreview
  :: t
  -> Getting (First a) s a
  -> s
  -> Either t a
taggedPreview tag l s = Err.note tag $ preview l s

main :: IO ()
main = do
  progName <- getProgName
  Opt.execParser (cliParser progName version) >>= run
  where
    run :: Options -> IO ()
    run opts = do
      let config = Giphy.GiphyConfig apiKey
      let app = getApp opts
      resp <- Giphy.runGiphy app config

      -- Get the first result and turn the left side into a String error
      let fstRes = resp & _Right %~ taggedPreview "No results found." _head
                        & _Left  %~ T.pack . show
                        & join

      -- Turn the right hand side into an Either.
      let fstUrl = fstRes & _Right %~ taggedPreview "No images attached."
                                      ( Giphy.gifImages
                                      . at "original"
                                      . traverse
                                      . Giphy.imageUrl
                                      . traverse )
                                    & join

      -- TODO: Understand lens actions and perform the fetch + tuple transform
      --       through it.
      let doReq uri = Wreq.get uri >>= (\resp' -> return (uri, resp'))
      resp' <- sequence $ doReq <$> (show <$> fstUrl)

      case resp' of
        Right r -> uncurry (printGif opts) r
        Left e -> TIO.hPutStrLn stderr $ "Error: " <> e

    getApp :: Options -> Giphy.Giphy [Giphy.Gif]
    getApp opts =
      case optMode opts of
        OptSearch s -> searchApp s
        OptTranslate t -> translateApp t
        OptRandom r -> randomApp r

    printGif :: Options -> String -> Wreq.Response BSL.ByteString -> IO ()
    printGif opts uri r = do
      unless (optNoPreview opts) $ do
        render <- getImageRenderer
        BS8.putStrLn . render $ consoleImage True (r ^. Wreq.responseBody)
      putStrLn uri

translateApp :: T.Text -> Giphy.Giphy [Giphy.Gif]
translateApp q = do
  resp <- Giphy.translate $ Giphy.Phrase q
  return . pure $ resp ^. Giphy.translateItem

searchApp :: T.Text -> Giphy.Giphy [Giphy.Gif]
searchApp q = do
  resp <- Giphy.search $ Giphy.Query q
  return $ resp ^. Giphy.searchItems

randomApp :: Maybe T.Text -> Giphy.Giphy [Giphy.Gif]
randomApp q = do
  resp <- Giphy.random $ Giphy.Tag <$> q
  return . pure $ resp ^. Giphy.randomGifItem
