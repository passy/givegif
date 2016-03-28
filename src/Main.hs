{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Bifunctor          (second)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base64  as B64
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8   as BS8
import qualified Data.ByteString.Lazy    as BSL
import           Data.List               (isPrefixOf)
import qualified Data.Map.Strict         as M
import           Data.Maybe              (catMaybes)
import           Data.Monoid             (mempty, (<>))
import           Control.Applicative     (empty)
import           Rainbow
import qualified System.Environment      as Env

import qualified Rainbow.Translate       as RT

data ConsoleImage = ConsoleImage
  { ciInline              :: !Bool
  , ciImage               :: !ByteString
  , ciName                :: !(Maybe ByteString)
  , ciWidth               :: !(Maybe Int)
  , ciHeight              :: !(Maybe Int)
  , ciPreserveAspectRatio :: !(Maybe Bool)
  } deriving (Eq, Show)

consoleImage :: Bool -> ByteString -> ConsoleImage
consoleImage inline image = ConsoleImage { ciInline = inline
                                         , ciImage = image
                                         , ciName = empty
                                         , ciWidth = empty
                                         , ciHeight = empty
                                         , ciPreserveAspectRatio = empty
                                         }

esc :: ByteString
esc = "\ESC"

imageToMap :: ConsoleImage -> M.Map ByteString ByteString
imageToMap img = M.union initial extra
  where
    btoi :: Bool -> Int
    btoi b = if b then 1 else 0

    showPack :: Show a => a -> ByteString
    showPack = BS8.pack . show

    initial = M.singleton "inline" $ (showPack . btoi . ciInline) img
    extra = M.fromList $ filterSnd [ ("name", showPack <$> ciName img)
                                   , ("width", showPack <$> ciWidth img)
                                   , ("height", showPack <$> ciHeight img)
                                   , ("preserveAspectRatio", showPack . btoi <$> ciPreserveAspectRatio img)
                                   ]

    filterSnd :: [(a, Maybe b)] -> [(a, b)]
    filterSnd = catMaybes . (liftSnd <$>)

    liftSnd :: (a, Maybe b) -> Maybe (a, b)
    liftSnd (a, Just b) = Just (a, b)
    liftSnd _ = Nothing

-- TODO: Use a builder.
getImageRenderer :: IO (ConsoleImage -> ByteString)
getImageRenderer = do
  screen <- isScreen
  let pre = (if screen then screenPreamble else "") <> esc <> "]1337;File="
  let post = "\a" <> (if screen then screenPost else "")

  return $ renderImage pre post

  where
    screenPreamble = esc <> "tmux;" <> esc
    screenPost = esc <> "\\"

renderImage :: ByteString -> ByteString -> ConsoleImage -> ByteString
renderImage pre post img =
  let b64 = B64.encode (ciImage img)
      p   = imageToMap img
  in  pre <> params p <> ":" <> b64 <> post

params :: M.Map ByteString ByteString -> ByteString
params = M.foldlWithKey f mempty
  where f a k b = (if BS.null b then b else b <> ";") <> k <> "=" <> a

isScreen :: IO Bool
isScreen = isPrefixOf "screen" <$> Env.getEnv "TERM"

main :: IO ()
main = do
  b <- BS.readFile "/Users/phartig/Downloads/cute-unicorn-clipart-unicorn4.png"
  render <- getImageRenderer
  BS8.putStrLn . render $ consoleImage True b
