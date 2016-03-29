{-# LANGUAGE OverloadedStrings #-}

module Console where

import           Control.Applicative         (empty)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Builder     as B
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.Lazy.Char8  as BS8
import           Data.List                   (isPrefixOf)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (catMaybes)
import           Data.Monoid                 ((<>), mempty)
import qualified System.Environment          as Env

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

esc :: B.Builder
esc = B.char8 '\ESC'

imageToMap :: ConsoleImage -> M.Map ByteString ByteString
imageToMap img = M.union initial extra
  where
    btoi :: Bool -> Int
    btoi b = if b then 1 else 0

    showPack :: Show a => a -> ByteString
    showPack = BS8.pack . show

    initial = M.singleton "inline" $ (showPack . btoi . ciInline) img
    extra = M.fromList $ filterSnd [ ("name", ciName img)
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
  let pre = if screen then screenPreamble else mempty <> esc <> B.stringUtf8 "]1337;File="
  let post = B.char8 '\a' <> if screen then screenPost else mempty

  return $ renderImage pre post

  where
    screenPreamble = esc <> B.stringUtf8 "Ptmux;" <> esc
    screenPost = esc <> B.char8 '\\'

renderImage :: B.Builder -> B.Builder -> ConsoleImage -> ByteString
renderImage pre post img =
  let b64 = B.lazyByteString $ B64.encode (ciImage img)
      p   = imageToMap img
  in  B.toLazyByteString $ pre <> params p <> ":" <> b64 <> post

params :: M.Map ByteString ByteString -> B.Builder
params = snd . M.foldrWithKey' f (True, mempty)
  where
    f k a (empty, b) = let start = if empty then b else b <> B.char8 ';'
                           end   = B.lazyByteString k <> B.char8 '=' <> B.lazyByteString a
                       in (False, start <> end)

isScreen :: IO Bool
isScreen = isPrefixOf "screen" <$> Env.getEnv "TERM"
