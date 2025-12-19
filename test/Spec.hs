module Main where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Pattern Lisp Test Suite" $ do
    it "test framework is working" $ do
      True `shouldBe` True

