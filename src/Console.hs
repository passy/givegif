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
import           Data.Monoid                 ((<>))
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

-- TODO: Use a builder.
getImageRenderer :: IO (ConsoleImage -> ByteString)
getImageRenderer = do
  screen <- isScreen
  let pre = (if screen then screenPreamble else "") <> esc <> "]1337;File="
  let post = "\a" <> (if screen then screenPost else "")

  return $ renderImage pre post

  where
    screenPreamble = esc <> "Ptmux;" <> esc
    screenPost = esc <> "\\"

renderImage :: ByteString -> ByteString -> ConsoleImage -> ByteString
renderImage pre post img =
  let b64 = B64.encode (ciImage img)
      p   = imageToMap img
  in  pre <> params p <> ":" <> b64 <> post

params :: M.Map ByteString ByteString -> ByteString
params = M.foldrWithKey' f mempty
  where f k a b = (if BSL.null b then b else b <> ";") <> k <> "=" <> a

isScreen :: IO Bool
isScreen = isPrefixOf "screen" <$> Env.getEnv "TERM"
