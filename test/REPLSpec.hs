module REPLSpec (spec) where

import Test.Hspec
import Lisp.Syntax
import Lisp.Parser
import Lisp.Eval
import Lisp.Primitives
import qualified Data.Text as T
import qualified Data.Map as Map

-- | Test helper: Process a single REPL line and return (output, updated env, shouldContinue)
processREPLLine :: String -> Env -> (String, Env, Bool)
processREPLLine input env
  | input == ":quit" || input == ":q" = ("", env, False)
  | input == ":help" || input == ":h" = ("Pattern Lisp REPL\nCommands: :quit, :q, :help, :h\n", env, True)
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
    
    it "handles empty input" $ do
      let (output, _, shouldContinue) = processREPLLine "" initialEnv
      output `shouldBe` ""
      shouldContinue `shouldBe` True

