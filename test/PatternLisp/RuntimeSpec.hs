module PatternLisp.RuntimeSpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import PatternLisp.Runtime
import Pattern (Pattern)
import Pattern.Core (pattern)
import qualified Pattern.Core as PatternCore
import Subject.Core (Subject(..))
import qualified Subject.Core as SubjectCore
import qualified Subject.Value as SubjectValue
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Helper to create a simple Pattern Subject for testing
createTestPattern :: String -> Pattern Subject
createTestPattern s = pattern $ Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["String"]
  , properties = Map.fromList [("text", SubjectValue.VString s)]
  }

spec :: Spec
spec = describe "PatternLisp.Runtime - Pure Function State Transformation" $ do
  describe "Tool validation" $ do
    it "valid tool: lambda with single state parameter" $ do
      case parseExpr "(lambda (state) state)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case validateTool expr initialEnv of
          Left err -> fail $ "Validation error: " ++ show err
          Right val -> case val of
            VClosure (Closure ["state"] _ _) -> True `shouldBe` True
            _ -> fail $ "Expected VClosure with single 'state' parameter, got: " ++ show val
    
    it "invalid tool: wrong parameter count" $ do
      case parseExpr "(lambda (x y) x)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case validateTool expr initialEnv of
          Left (TypeMismatch _ _) -> True `shouldBe` True
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected TypeMismatch error for wrong parameter count"
    
    it "invalid tool: wrong parameter name" $ do
      case parseExpr "(lambda (x) x)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case validateTool expr initialEnv of
          Left (TypeMismatch _ _) -> True `shouldBe` True
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected TypeMismatch error for wrong parameter name"
    
    it "invalid tool: not a lambda" $ do
      case parseExpr "42" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case validateTool expr initialEnv of
          Left (TypeMismatch _ _) -> True `shouldBe` True
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected TypeMismatch error for non-lambda"
  
  describe "Tool execution" $ do
    it "tool execution: identity tool returns same state" $ do
      case parseExpr "(lambda (state) state)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case validateTool expr initialEnv of
          Left err -> fail $ "Validation error: " ++ show err
          Right toolVal -> do
            let inputState = createTestPattern "hello"
            case executeTool toolVal inputState initialEnv of
              Left err -> fail $ "Execution error: " ++ show err
              Right outputState -> do
                -- Extract values to compare
                let inputSubj = PatternCore.value inputState
                let outputSubj = PatternCore.value outputState
                labels inputSubj `shouldBe` labels outputSubj
                properties inputSubj `shouldBe` properties outputSubj
    
    it "tool execution: tool transforms state correctly" $ do
      -- Tool that adds a new element (simplified - using pattern-with)
      case parseExpr "(lambda (state) (pattern-with \"transformed\" '()))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case validateTool expr initialEnv of
          Left err -> fail $ "Validation error: " ++ show err
          Right toolVal -> do
            let inputState = createTestPattern "hello"
            case executeTool toolVal inputState initialEnv of
              Left err -> fail $ "Execution error: " ++ show err
              Right outputState -> do
                -- Verify output is a pattern
                case PatternCore.value outputState of
                  subj -> do
                    let subjLabels = labels subj
                    "String" `Set.member` subjLabels `shouldBe` True
    
    it "tool execution: tool returns non-pattern should error" $ do
      case parseExpr "(lambda (state) 42)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case validateTool expr initialEnv of
          Left err -> fail $ "Validation error: " ++ show err
          Right toolVal -> do
            let inputState = createTestPattern "hello"
            case executeTool toolVal inputState initialEnv of
              Left (TypeMismatch _ _) -> True `shouldBe` True
              Left err -> fail $ "Unexpected error: " ++ show err
              Right _ -> fail "Expected TypeMismatch error for non-pattern return"
    
    it "tool composition: sequential tool execution" $ do
      -- First tool: identity
      case parseExpr "(lambda (state) state)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr1 -> case validateTool expr1 initialEnv of
          Left err -> fail $ "Validation error: " ++ show err
          Right tool1 -> do
            -- Second tool: also identity
            case parseExpr "(lambda (state) state)" of
              Left err -> fail $ "Parse error: " ++ show err
              Right expr2 -> case validateTool expr2 initialEnv of
                Left err -> fail $ "Validation error: " ++ show err
                Right tool2 -> do
                  let inputState = createTestPattern "hello"
                  -- Execute first tool
                  case executeTool tool1 inputState initialEnv of
                    Left err -> fail $ "First execution error: " ++ show err
                    Right intermediateState -> do
                      -- Execute second tool on result
                      case executeTool tool2 intermediateState initialEnv of
                        Left err -> fail $ "Second execution error: " ++ show err
                        Right finalState -> do
                          -- Verify final state matches input (both are identity)
                          let inputSubj = PatternCore.value inputState
                          let finalSubj = PatternCore.value finalState
                          labels inputSubj `shouldBe` labels finalSubj
                          properties inputSubj `shouldBe` properties finalSubj

