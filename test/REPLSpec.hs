module REPLSpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import qualified Data.Text as T
import qualified Data.Map as Map

-- | Format a Value type for display in variable listing
formatValueType :: Value -> String
formatValueType (VNumber _) = "Number"
formatValueType (VString _) = "String"
formatValueType (VBool _) = "Bool"
formatValueType (VList _) = "List"
formatValueType (VPattern _) = "Pattern"
formatValueType (VClosure (Closure params _ _)) = "Closure(" ++ unwords params ++ ")"
formatValueType (VPrimitive _) = "Primitive"

-- | Format environment variables for display
formatEnv :: Env -> String
formatEnv env = 
  if Map.null env
    then "No variables in scope"
    else unlines $ map formatBinding $ Map.toAscList env
  where
    formatBinding (name, val) = 
      "  " ++ name ++ " : " ++ formatValueType val

-- | Test helper: Process a single REPL line and return (output, updated env, shouldContinue)
processREPLLine :: String -> Env -> (String, Env, Bool)
processREPLLine input env
  | input == ":quit" || input == ":q" = ("", env, False)
  | input == ":help" || input == ":h" = ("Pattern Lisp REPL\nCommands:\n  :quit, :q        Exit REPL\n  :help, :h         Show this help\n  :vars, :v        Show variables in scope\n\n  Execute tools: (tool-name state-name)\n", env, True)
  | input == ":vars" || input == ":v" = ("Variables in scope:\n" ++ formatEnv env ++ "\n", env, True)
  | null (trim input) = ("", env, True)
  | otherwise = case parseExpr input of
      Left err -> ("Parse error: " ++ show err ++ "\n", env, True)
      Right expr -> case evalExprWithEnv expr env of
        Left evalErr -> ("Error: " ++ show evalErr ++ "\n", env, True)
        Right (val, newEnv) -> (show val ++ "\n", newEnv, True)
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

spec :: Spec
spec = describe "REPL" $ do
  describe "Basic functionality" $ do
    it "parses and evaluates simple expression" $ do
      let (output, _, _) = processREPLLine "(+ 1 2)" initialEnv
      output `shouldBe` "VNumber 3\n"
    
    it "handles define and uses defined variable" $ do
      let (output1, env1, _) = processREPLLine "(define x 10)" initialEnv
      output1 `shouldBe` "VString \"x\"\n"
      let (output2, _, _) = processREPLLine "(+ x 5)" env1
      output2 `shouldBe` "VNumber 15\n"
    
    it "maintains environment across iterations" $ do
      let (_, env1, _) = processREPLLine "(define x 10)" initialEnv
      let (_, env2, _) = processREPLLine "(define y 20)" env1
      let (output, _, _) = processREPLLine "(+ x y)" env2
      output `shouldBe` "VNumber 30\n"
  
  describe "Error handling" $ do
    it "handles parse errors gracefully" $ do
      let (output, _, shouldContinue) = processREPLLine "(unclosed" initialEnv
      output `shouldContain` "Parse error"
      shouldContinue `shouldBe` True
    
    it "handles evaluation errors gracefully" $ do
      let (output, _, shouldContinue) = processREPLLine "undefined-var" initialEnv
      output `shouldContain` "Error"
      shouldContinue `shouldBe` True
    
    it "continues after errors" $ do
      let (_, env1, _) = processREPLLine "undefined-var" initialEnv
      let (output, _, _) = processREPLLine "(+ 1 2)" env1
      output `shouldBe` "VNumber 3\n"
  
  describe "Commands" $ do
    it "handles :quit command" $ do
      let (output, _, shouldContinue) = processREPLLine ":quit" initialEnv
      shouldContinue `shouldBe` False
    
    it "handles :q command" $ do
      let (_, _, shouldContinue) = processREPLLine ":q" initialEnv
      shouldContinue `shouldBe` False
    
    it "handles :help command" $ do
      let (output, _, shouldContinue) = processREPLLine ":help" initialEnv
      output `shouldContain` "Pattern Lisp REPL"
      shouldContinue `shouldBe` True
    
    it "handles :h command" $ do
      let (output, _, shouldContinue) = processREPLLine ":h" initialEnv
      output `shouldContain` "Pattern Lisp REPL"
      shouldContinue `shouldBe` True
    
    it "handles :vars command" $ do
      let (output, _, shouldContinue) = processREPLLine ":vars" initialEnv
      output `shouldContain` "Variables in scope"
      output `shouldContain` "Primitive"  -- Should show primitives
      shouldContinue `shouldBe` True
    
    it "handles :v command" $ do
      let (output, _, shouldContinue) = processREPLLine ":v" initialEnv
      output `shouldContain` "Variables in scope"
      shouldContinue `shouldBe` True
    
    it "shows user-defined variables in :vars" $ do
      let (_, env1, _) = processREPLLine "(define x 10)" initialEnv
      let (output, _, shouldContinue) = processREPLLine ":vars" env1
      output `shouldContain` "x : Number"
      shouldContinue `shouldBe` True
    
    it "handles empty input" $ do
      let (output, _, shouldContinue) = processREPLLine "" initialEnv
      output `shouldBe` ""
      shouldContinue `shouldBe` True

