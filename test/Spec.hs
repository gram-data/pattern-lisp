module Main where

import Test.Hspec
import qualified PatternLisp.ParserSpec
import qualified PatternLisp.PrimitivesSpec
import qualified PatternLisp.EvalSpec
import qualified PatternLisp.PatternSpec
import qualified PatternLisp.RuntimeSpec
import qualified PatternLisp.CodecSpec
import qualified PatternLisp.ConvertSpec
import qualified PatternLisp.GramSpec
import qualified PatternLisp.GramSerializationSpec
import qualified PatternLisp.RecordGramCompatibilitySpec
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
    PatternLisp.PatternSpec.spec
    PatternLisp.RuntimeSpec.spec
    PatternLisp.CodecSpec.spec
    PatternLisp.ConvertSpec.spec
    PatternLisp.GramSpec.spec
    PatternLisp.GramSerializationSpec.spec
    PatternLisp.RecordGramCompatibilitySpec.spec
    REPLSpec.spec
    Properties.spec
    ExamplesSpec.spec
    IntegrationSpec.spec

