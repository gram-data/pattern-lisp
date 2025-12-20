module Main where

import Test.Hspec
import qualified Lisp.ParserSpec
import qualified Lisp.PrimitivesSpec
import qualified Lisp.EvalSpec
import qualified REPLSpec
import qualified Properties
import qualified ExamplesSpec
import qualified IntegrationSpec

main :: IO ()
main = hspec $ do
  describe "Pattern Lisp Test Suite" $ do
    Lisp.ParserSpec.spec
    Lisp.PrimitivesSpec.spec
    Lisp.EvalSpec.spec
    REPLSpec.spec
    Properties.spec
    ExamplesSpec.spec
    IntegrationSpec.spec

