{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}

import           Prelude
import           Test.Hspec
import qualified Console as C
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString         as BS

main :: IO ()
main = hspec $ do
  describe "Console Module" $ do
    describe "imageToMap" $ do
      let img = (C.consoleImage True "data") { C.ciName = pure "myimage.png" }
      it "has the right keys" $ do
        let m = C.imageToMap img
        M.keysSet m `shouldBe` S.fromList [ "inline", "name" ]
      it "creates the right params" $ do
        let p = C.params . C.imageToMap $ img
        p `shouldBe` "123"
    describe "renderImage" $ do
      let img = C.consoleImage True ""
      it "renders an image without pre/post" $ do
        C.renderImage "" "" img `shouldBe` ""
