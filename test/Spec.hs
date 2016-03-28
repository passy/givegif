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
      let img = (C.consoleImage True "data") { C.ciName = pure "myimage.png"
                                             , C.ciPreserveAspectRatio = pure True }
      it "has the right keys" $ do
        let m = C.imageToMap img
        M.keysSet m `shouldBe` S.fromList [ "inline", "name", "preserveAspectRatio" ]
      it "creates the right params" $ do
        let p = C.params . C.imageToMap $ img
        p `shouldBe` "preserveAspectRatio=1;name=myimage.png;inline=1"
    describe "renderImage" $ do
      it "renders an image without pre/post" $ do
        let img = C.consoleImage True ""
        C.renderImage "" "" img `shouldBe` "inline=1:"
      it "renders an image with pre/post" $ do
        let img = C.consoleImage True "data"
        C.renderImage "pre" "post" img `shouldBe` "preinline=1:ZGF0YQ==post"
