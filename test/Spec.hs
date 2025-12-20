module Main where

import Test.Hspec
import qualified PatternLisp.ParserSpec
import qualified PatternLisp.PrimitivesSpec
import qualified PatternLisp.EvalSpec
import qualified REPLSpec
import qualified Properties
import qualified ExamplesSpec
import qualified IntegrationSpec

main :: IO ()
main = hspec $ do
  describe "Pattern Lisp Test Suite" $ do
    PatternLisp.ParserSpec.spec
    PatternLisp.PrimitivesSpec.spec
    PatternLisp.EvalSpec.spec
    REPLSpec.spec
    Properties.spec
    ExamplesSpec.spec
    IntegrationSpec.spec

