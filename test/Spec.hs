module Main where

import Test.Hspec
import qualified Lisp.ParserSpec
import qualified Lisp.PrimitivesSpec
import qualified Lisp.EvalSpec

main :: IO ()
main = hspec $ do
  describe "Pattern Lisp Test Suite" $ do
    Lisp.ParserSpec.spec
    Lisp.PrimitivesSpec.spec
    Lisp.EvalSpec.spec

