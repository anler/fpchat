{-# LANGUAGE OverloadedStrings #-}

module Network.ChatSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "Prelude.head" $ do
    describe "Nested" $ do
      it "returns first element" $ do
        head [1..] `shouldBe` (1 :: Int)

      it "fails badly" $ do
        head [1..] `shouldBe` (3 :: Int)
