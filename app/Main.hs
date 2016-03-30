{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Text                  as T
import qualified Network.Wreq               as Wreq
import qualified Options.Applicative        as Opt
import qualified Options.Applicative.Types  as Opt
import qualified Web.Giphy                  as Giphy

import           Control.Applicative        (optional, (<**>), (<|>))
import           Control.Lens.At            (at)
import           Control.Lens.Cons          (_head)
import           Control.Lens.Operators
import           Control.Lens.Prism         (_Right)
import           Data.Monoid                ((<>))
import           Data.Version               (Version (), showVersion)
import           Paths_givegif              (version)
import           System.Environment         (getProgName)

import           Console

data Options = Options
  { optShowImage :: Bool
  , optMode :: SearchMode
  }

data SearchMode = OptSearch T.Text | OptTranslate T.Text | OptRandom (Maybe T.Text)

options :: Opt.Parser Options
options =
  Options <$> Opt.switch ( Opt.long "no-preview"
                        <> Opt.short 'p'
                        <> Opt.help "Don't render an inline image preview." )
          <*> ( ( OptSearch <$> textOption ( Opt.long "search"
                                          <> Opt.short 's'
                                          <> Opt.help "Use search to find a matching GIF." ) )
          <|> ( OptTranslate <$> textOption ( Opt.long "translate"
                                           <> Opt.short 't'
                                           <> Opt.help "Use translate to find a matching GIF." ) )
          <|> ( OptRandom <$> optional ( textArgument ( Opt.metavar "RANDOM_TAG" ) ) ) )
  where
    -- TODO: This seems quite useful. Maybe publish as Options.Applicative.Text?
    text :: Opt.ReadM T.Text
    text = T.pack <$> Opt.readerAsk

    textOption :: Opt.Mod Opt.OptionFields T.Text -> Opt.Parser T.Text
    textOption = Opt.option text

    textArgument :: Opt.Mod Opt.ArgumentFields T.Text -> Opt.Parser T.Text
    textArgument = Opt.argument text

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
      -- TODO: Funnel the functor through this, rather than reducing it
      -- to a Maybe. I.e. maintain the Left somehow.
      let fstUrl = resp ^? _Right
                         . _head
                         . Giphy.gifImages
                         . at "original"
                         . traverse
                         . Giphy.imageUrl
                         . traverse

      resp' <- sequence $ Wreq.get <$> (show <$> fstUrl)
      case resp' of
        Just r -> printGif r
        Nothing -> BS8.putStrLn "error."

    getApp :: Options -> Giphy.Giphy [Giphy.Gif]
    getApp opts =
      case optMode opts of
        OptSearch s -> searchApp s
        OptTranslate t -> translateApp t
        OptRandom r -> randomApp r

    printGif :: Wreq.Response BSL.ByteString -> IO ()
    printGif r = do
      render <- getImageRenderer
      BS8.putStrLn . render $ consoleImage True (r ^. Wreq.responseBody)

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
