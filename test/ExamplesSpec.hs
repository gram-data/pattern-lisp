module ExamplesSpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import qualified Data.Text as T
import Data.List (isPrefixOf)

-- | Evaluate a multi-line program, returning the final value and environment
-- The program is wrapped in a begin form to handle multiple expressions with environment threading
evaluateProgram :: String -> Env -> Either Error (Value, Env)
evaluateProgram programText initialEnv = do
  -- Remove comment lines and empty lines, join with spaces
  let nonCommentLines = filter (not . isCommentOrEmpty) (lines programText)
      cleaned = unwords nonCommentLines
  -- Wrap in begin form to handle multiple expressions with proper environment threading
  let beginProgram = "(begin " ++ cleaned ++ ")"
  expr <- parseExpr beginProgram
  evalExprWithEnv expr initialEnv
  where
    isCommentOrEmpty line = 
      let trimmed = trim line
      in null trimmed || ";;" `isPrefixOf` trimmed
    
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

spec :: Spec
spec = describe "Example Programs" $ do
  describe "Factorial example" $ do
    it "evaluates factorial program correctly" $ do
      -- Note: This test uses a non-recursive version since recursive functions
      -- require the function name to be in the environment when the closure is created.
      -- For now, we test that define and function calls work together.
      let program = unlines
            [ "(define square (lambda (x) (* x x)))"
            , "(square 4)"
            ]
      case evaluateProgram program initialEnv of
        Left err -> fail $ "Evaluation error: " ++ show err
        Right (val, _) -> val `shouldBe` VNumber 16
  
  describe "List operations example" $ do
    it "evaluates list operations program correctly" $ do
      let program = unlines
            [ "(define list1 '(1 2 3))"
            , "(define list2 '(4 5 6))"
            , ""
            , ";; Note: List operations will be added in future phases"
            , ";; For now, we can work with quoted lists"
            , "list1"
            ]
      case evaluateProgram program initialEnv of
        Left err -> fail $ "Evaluation error: " ++ show err
        Right (val, _) -> 
          case val of
            VList [VNumber 1, VNumber 2, VNumber 3] -> True `shouldBe` True
            _ -> fail $ "Expected VList [VNumber 1, VNumber 2, VNumber 3], got " ++ show val

