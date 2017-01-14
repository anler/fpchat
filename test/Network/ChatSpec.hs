{-# LANGUAGE OverloadedStrings #-}

module Network.ChatSpec where

import Text.Parsec (parse)
import Data.Either (isLeft)

import qualified Network.Chat.Parsers as P
import Network.Chat.Commands

import Test.Hspec

parses = (`parse` "")

spec :: Spec
spec = do
  parserSpecs

parserSpecs :: Spec
parserSpecs = do
  describe "Commands parser" $ do
    describe "nick" $ do
      describe "getter" $ do
        it "should match exactly" $ do
          parses P.nick "nick" `shouldBe` Right (AskNick)

        it "should ignore trailing whitespace" $ do
          parses P.nick "nick   " `shouldBe` Right (AskNick)

        it "should fail if malformed" $ do
          parses P.nick "nickk" `shouldSatisfy` isLeft

      describe "setter" $ do
        it "should match exactly" $ do
          parses P.nick "nick username" `shouldBe` Right (SetNick "username")

        it "should ignore extra whitespace" $ do
          parses P.nick "nick    username" `shouldBe` Right (SetNick "username")

        it "should fail if malformed" $ do
          parses P.nick "nick username lastname" `shouldSatisfy` isLeft

    describe "msg" $ do
      it "should match exactly" $ do
        parses P.msg "msg hello there" `shouldBe` Right (Msg "hello there")

      it "should fail if malformed" $ do
        parses P.msg "msgg hello there" `shouldSatisfy` isLeft

    describe "names" $ do
      it "should match exactly" $ do
        parses P.names "names" `shouldBe` Right Names

      it "should fail if malformed" $ do
        parses P.names "namess foo" `shouldSatisfy` isLeft

    describe "quit" $ do
      describe "without parting message" $ do
        it "should match exactly" $ do
          parses P.quit "quit" `shouldBe` Right (Quit Nothing)

        it "should fail if malformed" $ do
          parses P.quit "quitt" `shouldSatisfy` isLeft

      describe "with parting message" $ do
        it "should match exactly" $ do
          parses P.quit "quit hasta la vista" `shouldBe` Right (Quit (Just "hasta la vista"))

    describe "kick" $ do
      it "should match exactly" $ do
        parses P.kick "kick username" `shouldBe` Right (Kick "username")

      it "should fail if malformed" $ do
        parses P.kick "kickk" `shouldSatisfy` isLeft

