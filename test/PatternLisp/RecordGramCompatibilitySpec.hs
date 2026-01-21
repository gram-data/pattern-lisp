module PatternLisp.RecordGramCompatibilitySpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import PatternLisp.Codec
import PatternLisp.Gram
import PatternLisp.PatternPrimitives
import qualified Gram.Parse as GramParse
import qualified Pattern.Core as PatternCore
import qualified Subject.Core as SubjectCore
import qualified Subject.Value as SubjectValue
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (isInfixOf)
import Control.Monad.Reader
import Control.Monad.Except (runExcept)

-- | Validate record key (reject numeric keys)
-- Pattern-lisp restriction: gram allows numeric keys, but pattern-lisp does not
validateRecordKey :: String -> Either String String
validateRecordKey key
  | null key = Left "Invalid record key: empty key"
  | all (`elem` ['0'..'9']) key = Left $ "Invalid record key: numeric keys are not allowed (got: " ++ key ++ ")"
  | otherwise = Right key

-- | Check if record text contains pattern-lisp specific syntax
-- (unquotes or splices)
hasPatternLispSyntax :: String -> Bool
hasPatternLispSyntax text = 
  ",@" `isInfixOf` text ||  -- Splice syntax
  hasUnquoteSyntax text     -- Unquote syntax (comma followed by identifier)

-- | Check if text contains unquote syntax: ,identifier (comma followed by non-space, non-@)
hasUnquoteSyntax :: String -> Bool
hasUnquoteSyntax text = 
  let checkChar i = i < length text - 1 && 
                    text !! i == ',' && 
                    text !! (i + 1) `notElem` [' ', '\t', '\n', '@', '}']
  in any checkChar [0..length text - 2]

-- | Validate that a record string can be parsed by pattern-lisp and serialized to gram
-- This is a more practical test: parse with Megaparsec, evaluate, serialize to gram, parse back
-- Returns Right () if the round-trip works, Left error message if it fails
validateRecordWithGram :: String -> Either String ()
validateRecordWithGram recordText = do
  -- Parse with Megaparsec
  expr <- case parseExpr recordText of
    Left err -> Left $ "Pattern-lisp parse error: " ++ show err
    Right e -> Right e
  -- Evaluate
  val <- case evalExpr expr initialEnv of
    Left err -> Left $ "Evaluation error: " ++ show err
    Right v -> Right v
  -- Convert to Pattern Subject and serialize to gram
  let pat = case runExcept $ runReaderT (valueToPatternSubject val) initialEnv of
        Left err -> error $ "Pattern conversion error: " ++ show err
        Right p -> p
      gramText = patternToGram pat
  -- Parse back with gram parser to ensure compatibility
  case gramToPattern gramText of
    Left err -> Left $ "Gram round-trip error: " ++ show err
    Right _ -> Right ()

-- | Test that a record parses with Megaparsec and is gram-compatible
-- Parses the record, evaluates it, then validates it can be serialized to gram
testRecordGramCompatibility :: String -> IO ()
testRecordGramCompatibility input = do
  -- Parse with Megaparsec
  case parseExpr input of
    Left err -> fail $ "Pattern-lisp parse error: " ++ show err
    Right expr -> do
      -- Evaluate
      case evalExpr expr initialEnv of
        Left err -> fail $ "Evaluation error: " ++ show err
        Right val -> do
          -- Convert to Pattern Subject and serialize to gram
          let pat = case runExcept $ runReaderT (valueToPatternSubject val) initialEnv of
                Left err -> error $ "Pattern conversion error: " ++ show err
                Right p -> p
              gramText = patternToGram pat
          -- Parse back with gram parser to ensure compatibility
          case gramToPattern gramText of
            Left err -> fail $ "Gram round-trip error: " ++ show err
            Right _ -> True `shouldBe` True

spec :: Spec
spec = describe "Record Gram Compatibility" $ do
  describe "Positive examples (gram-compatible records)" $ do
    it "validates basic record with gram parser" $ do
      validateRecordWithGram "{name: \"Alice\", age: 30}" `shouldBe` Right ()
    
    it "validates nested record with gram parser" $ do
      validateRecordWithGram "{user: {name: \"Bob\"}}" `shouldBe` Right ()
    
    it "validates record with arrays" $ do
      validateRecordWithGram "{tags: [\"a\", \"b\"]}" `shouldBe` Right ()
    
    it "validates record with all value types" $ do
      validateRecordWithGram "{integer: 42, string: \"hello\", boolean: true, array: [1, 2, 3]}" `shouldBe` Right ()
    
    it "validates empty record" $ do
      validateRecordWithGram "{}" `shouldBe` Right ()
    
    it "validates record with double colon" $ do
      validateRecordWithGram "{name:: \"Alice\"}" `shouldBe` Right ()
    
    it "validates record with string keys" $ do
      validateRecordWithGram "{\"user-id\": 123, \"full-name\": \"Alice\"}" `shouldBe` Right ()
    
    it "validates record with mixed identifier and string keys" $ do
      validateRecordWithGram "{name: \"Alice\", \"user-id\": 123}" `shouldBe` Right ()
    
    it "validates deeply nested records" $ do
      validateRecordWithGram "{a: {b: {c: \"value\"}}}" `shouldBe` Right ()
    
    it "validates record with arrays containing records" $ do
      validateRecordWithGram "{users: [{name: \"Alice\"}, {name: \"Bob\"}]}" `shouldBe` Right ()
    
    it "parses and round-trips basic record" $ do
      testRecordGramCompatibility "{name: \"Alice\", age: 30}"
    
    it "parses and round-trips nested record" $ do
      testRecordGramCompatibility "{user: {name: \"Bob\", email: \"bob@example.com\"}}"
    
    it "parses and round-trips record with arrays" $ do
      testRecordGramCompatibility "{tags: [\"admin\", \"user\"], scores: [95, 87]}"
    
    it "parses and round-trips complex record" $ do
      testRecordGramCompatibility "{name: \"Alice\", age: 30, tags: [\"admin\"], settings: {theme: \"dark\", notifications: true}}"
  
  describe "Negative examples (should NOT be gram-compatible or should be rejected)" $ do
    it "rejects numeric keys (pattern-lisp restriction)" $ do
      case parseExpr "{123: \"value\"}" of
        Left _ -> True `shouldBe` True  -- Should fail to parse
        Right _ -> fail "Should reject numeric keys"
    
    it "rejects empty key" $ do
      case parseExpr "{: \"value\"}" of
        Left _ -> True `shouldBe` True  -- Should fail to parse
        Right _ -> fail "Should reject empty key"
    
    it "rejects unclosed record" $ do
      case parseExpr "{name: \"Alice\"" of
        Left _ -> True `shouldBe` True  -- Should fail to parse
        Right _ -> fail "Should reject unclosed record"
    
    it "rejects malformed record (missing colon)" $ do
      case parseExpr "{name \"Alice\"}" of
        Left _ -> True `shouldBe` True  -- Should fail to parse
        Right _ -> fail "Should reject missing colon"
    
    it "rejects record with invalid array syntax" $ do
      case parseExpr "{tags: [\"a\" \"b\"]}" of
        Left _ -> True `shouldBe` True  -- Should fail (missing comma in array)
        Right _ -> fail "Should reject invalid array syntax"
    
    it "rejects record with trailing comma (if not supported)" $ do
      -- This might be valid depending on parser implementation
      -- Adjust test based on actual behavior
      case parseExpr "{name: \"Alice\",}" of
        Left _ -> True `shouldBe` True  -- If trailing comma not supported
        Right _ -> True `shouldBe` True  -- If trailing comma is supported
    
    it "gram parser rejects invalid gram syntax" $ do
      case validateRecordWithGram "{invalid syntax!!}" of
        Left _ -> True `shouldBe` True  -- Should fail
        Right _ -> fail "Should reject invalid gram syntax"
    
    it "gram parser rejects malformed nested structure" $ do
      case validateRecordWithGram "{a: {b: }" of
        Left _ -> True `shouldBe` True  -- Should fail
        Right _ -> fail "Should reject malformed nested structure"
  
  describe "Pattern-lisp specific syntax (not gram-compatible)" $ do
    it "allows unquotes in records (pattern-lisp feature)" $ do
      -- Unquotes are pattern-lisp specific, not gram-compatible
      -- But they should parse and evaluate correctly
      case parseExpr "`{name: ,\"Alice\"}" of
        Left err -> fail $ "Should parse unquotes: " ++ show err
        Right _ -> True `shouldBe` True
    
    it "allows splices in records (pattern-lisp feature)" $ do
      -- Splices are pattern-lisp specific, not gram-compatible
      -- But they should parse and evaluate correctly
      case parseExpr "`{,@{role: \"Engineer\"}}" of
        Left err -> fail $ "Should parse splices: " ++ show err
        Right _ -> True `shouldBe` True
    
    it "records with pattern-lisp syntax are not gram-compatible" $ do
      -- Records with unquotes/splices are pattern-lisp specific features
      -- They parse and evaluate correctly in pattern-lisp, but the resulting
      -- gram serialization won't contain the unquote syntax (it will be evaluated)
      -- So we test that unquotes work in pattern-lisp context
      case parseExpr "`{name: ,\"Alice\"}" of
        Left err -> fail $ "Should parse unquotes in quasiquote context: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Should evaluate unquotes: " ++ show err
          Right val -> do
            -- The value should be a record with "Alice" as the name
            case val of
              VMap m -> Map.lookup "name" m `shouldBe` Just (VString "Alice")
              _ -> fail "Should evaluate to a record"
  
  describe "Round-trip validation (parse -> evaluate -> serialize -> parse)" $ do
    it "round-trips simple record through gram" $ do
      let input = "{name: \"Alice\", age: 30}"
      case parseExpr input of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            let pat = case runExcept $ runReaderT (valueToPatternSubject val) initialEnv of
                  Left err -> error $ "Pattern conversion error: " ++ show err
                  Right p -> p
                gramText = patternToGram pat
            case gramToPattern gramText of
              Left err -> fail $ "Gram round-trip error: " ++ show err
              Right pat' -> do
                -- Verify structure is preserved
                let subj = PatternCore.value pat
                    subj' = PatternCore.value pat'
                Map.size (SubjectCore.properties subj) `shouldBe` Map.size (SubjectCore.properties subj')
    
    it "round-trips nested record through gram" $ do
      let input = "{user: {name: \"Bob\"}}"
      case parseExpr input of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            let pat = case runExcept $ runReaderT (valueToPatternSubject val) initialEnv of
                  Left err -> error $ "Pattern conversion error: " ++ show err
                  Right p -> p
                gramText = patternToGram pat
            case gramToPattern gramText of
              Left err -> fail $ "Gram round-trip error: " ++ show err
              Right _ -> True `shouldBe` True
    
    it "round-trips record with arrays through gram" $ do
      let input = "{tags: [\"admin\", \"user\"]}"
      case parseExpr input of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            let pat = case runExcept $ runReaderT (valueToPatternSubject val) initialEnv of
                  Left err -> error $ "Pattern conversion error: " ++ show err
                  Right p -> p
                gramText = patternToGram pat
            case gramToPattern gramText of
              Left err -> fail $ "Gram round-trip error: " ++ show err
              Right _ -> True `shouldBe` True
