module PatternLisp.GramSpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import PatternLisp.PatternPrimitives
import PatternLisp.Gram
import PatternLisp.Codec
import Pattern (Pattern)
import Pattern.Core (pattern, patternWith)
import qualified Pattern.Core as PatternCore
import Subject.Core (Subject(..), Symbol(..))
import qualified Subject.Core as SubjectCore
import qualified Subject.Value as SubjectValue
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except (runExcept)

-- | Helper to create a simple test pattern
createTestPattern :: String -> Pattern Subject
createTestPattern str = pattern $ SubjectCore.Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["String"]
  , properties = Map.fromList [("text", SubjectValue.VString str)]
  }

spec :: Spec
spec = describe "PatternLisp.Gram - Gram Serialization" $ do
  describe "patternToGram and gramToPattern round-trips" $ do
    it "round-trips simple atomic pattern" $ do
      let pat = createTestPattern "hello"
          gramText = patternToGram pat
      case gramToPattern gramText of
        Left err -> fail $ "Parse error: " ++ show err
        Right pat' -> do
          -- Gram may assign identities, so check labels and properties instead
          let subj = PatternCore.value pat
              subj' = PatternCore.value pat'
          labels subj' `shouldBe` labels subj
          properties subj' `shouldBe` properties subj
          length (PatternCore.elements pat') `shouldBe` length (PatternCore.elements pat)
    
    it "round-trips pattern with elements" $ do
      let decoration = SubjectCore.Subject
            { identity = SubjectCore.Symbol "root"
            , labels = Set.fromList ["String"]
            , properties = Map.fromList [("text", SubjectValue.VString "root")]
            }
          element1 = createTestPattern "child1"
          element2 = createTestPattern "child2"
          pat = patternWith decoration [element1, element2]
          gramText = patternToGram pat
      case gramToPattern gramText of
        Left err -> fail $ "Parse error: " ++ show err
        Right pat' -> do
          -- Check structure matches
          PatternCore.value pat' `shouldBe` decoration
          length (PatternCore.elements pat') `shouldBe` 2
  
  describe "valueToPatternSubject" $ do
    it "maps VNumber to Pattern Subject" $ do
      let val = VNumber 42
          env = initialEnv
      case runExcept $ runReaderT (valueToPatternSubject val) env of
        Left err -> fail $ "Error: " ++ show err
        Right pat -> do
          let subj = PatternCore.value pat
          "Number" `Set.member` labels subj `shouldBe` True
    
    it "maps VString to Pattern Subject" $ do
      let val = VString (T.pack "hello")
          env = initialEnv
      case runExcept $ runReaderT (valueToPatternSubject val) env of
        Left err -> fail $ "Error: " ++ show err
        Right pat -> do
          let subj = PatternCore.value pat
          "String" `Set.member` labels subj `shouldBe` True
    
    it "maps VBool to Pattern Subject" $ do
      let val = VBool True
          env = initialEnv
      case runExcept $ runReaderT (valueToPatternSubject val) env of
        Left err -> fail $ "Error: " ++ show err
        Right pat -> do
          let subj = PatternCore.value pat
          "Bool" `Set.member` labels subj `shouldBe` True
    
    it "maps VList to Pattern Subject with elements" $ do
      let val = VList [VNumber 1, VNumber 2, VNumber 3]
          env = initialEnv
      case runExcept $ runReaderT (valueToPatternSubject val) env of
        Left err -> fail $ "Error: " ++ show err
        Right pat -> do
          let subj = PatternCore.value pat
          "List" `Set.member` labels subj `shouldBe` True
          length (PatternCore.elements pat) `shouldBe` 3
    
    it "maps empty VList to atomic Pattern Subject" $ do
      let val = VList []
          env = initialEnv
      case runExcept $ runReaderT (valueToPatternSubject val) env of
        Left err -> fail $ "Error: " ++ show err
        Right pat -> do
          let subj = PatternCore.value pat
          "List" `Set.member` labels subj `shouldBe` True
          length (PatternCore.elements pat) `shouldBe` 0
    
    it "maps VPattern to Pattern Subject (identity)" $ do
      let inputPat = createTestPattern "test"
          val = VPattern inputPat
          env = initialEnv
      case runExcept $ runReaderT (valueToPatternSubject val) env of
        Left err -> fail $ "Error: " ++ show err
        Right pat -> pat `shouldBe` inputPat
  
  describe "Subject Labels as String Sets" $ do
    it "verifies string sets can be used directly for gram pattern Subject labels" $ do
      -- Test that Subject labels are Set String and can be used with gram patterns
      -- This demonstrates that #{"Person" "Employee"} conceptually represents Subject labels
      let labelSet = Set.fromList ["Person", "Employee"]  -- Set String (Subject labels type)
          subject = SubjectCore.Subject
            { identity = SubjectCore.Symbol "user-123"
            , labels = labelSet
            , properties = Map.fromList [("name", SubjectValue.VString "Alice")]
            }
          pat = pattern subject
          gramText = patternToGram pat
      -- Round-trip through gram notation
      case gramToPattern gramText of
        Left err -> fail $ "Gram parse error: " ++ show err
        Right pat' -> do
          let subj' = PatternCore.value pat'
          -- Verify labels (Set String) are preserved through gram serialization
          labels subj' `shouldBe` labelSet
          properties subj' `shouldBe` properties subject
          -- Verify that labels are indeed a Set String (unordered, unique)
          Set.size (labels subj') `shouldBe` 2
          "Person" `Set.member` (labels subj') `shouldBe` True
          "Employee" `Set.member` (labels subj') `shouldBe` True

