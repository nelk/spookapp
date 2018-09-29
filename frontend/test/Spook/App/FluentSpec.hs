{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spook.App.FluentSpec where

import qualified Data.Map         as Map
import qualified Reflex.Spider    as R
import           Test.Hspec

import           Spook.App.Fluent

spec :: Spec
spec = do
  describe "CssClass" $
    it "should be hold text" $ do
      "test" `shouldBe` CssClass "test"
      unCssClass (CssClass "test") `shouldBe` "test"
  describe "Attr" $
    it "should combine things" $ do
      let (Attrs{_attrsStatic = m} :: Attrs R.Spider) = () <.> () in m `shouldBe` Map.empty
      let (Attrs{_attrsStatic = m} :: Attrs R.Spider) = () <.> CssClass "test" in m `shouldBe` Map.singleton "class" "test"
