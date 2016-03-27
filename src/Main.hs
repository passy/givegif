{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import Data.Bifunctor (second)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as BS8
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Map.Strict        as M
import           Data.Monoid
import           Rainbow
import Data.Maybe (catMaybes)

import qualified Rainbow.Translate      as RT

data ConsoleImage = ConsoleImage
  { ciInline              :: !Bool
  , ciImage               :: !ByteString
  , ciName                :: !(Maybe ByteString)
  , ciWidth               :: !(Maybe Int)
  , ciHeight              :: !(Maybe Int)
  , ciPreserveAspectRatio :: !(Maybe Bool)
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

params :: (Show a, Show b) => M.Map a b -> ByteString
params = undefined

isScreen :: IO Bool
isScreen = undefined

main :: IO ()
main = do
  b <- b64 "/Users/phartig/Downloads/cute-unicorn-clipart-unicorn4.png"
  BS8.putStrLn $ "\ESC]1337;File=inline=1:" <> b <> "\a"
  where
    b64 f = B64.encode <$> BS.readFile f
