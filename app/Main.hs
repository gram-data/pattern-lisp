module Main where

import Lisp.Parser
import Lisp.Eval
import Lisp.Primitives
import Lisp.Syntax
import System.IO
import qualified Data.Text as T

-- | Format a Value for display
formatValue :: Value -> String
formatValue (VNumber n) = show n
formatValue (VString s) = T.unpack s
formatValue (VBool True) = "#t"
formatValue (VBool False) = "#f"
formatValue (VList vals) = "(" ++ unwords (map formatValue vals) ++ ")"
formatValue (VClosure _) = "<closure>"
formatValue (VPrimitive _) = "<primitive>"

-- | Format an Error for display
formatError :: Error -> String
formatError (UndefinedVar name _) = "Error: Undefined variable: " ++ name
formatError (TypeMismatch msg _) = "Error: Type mismatch: " ++ msg
formatError (ArityMismatch name expected actual) = 
  "Error: Arity mismatch in " ++ name ++ ": expected " ++ show expected ++ 
  " arguments, got " ++ show actual
formatError (DivisionByZero _) = "Error: Division by zero"
formatError (ParseError msg) = "Parse error: " ++ msg

-- | Process a single REPL line
processLine :: String -> Env -> IO (Env, Bool)
processLine input env
  | input == ":quit" || input == ":q" = return (env, False)
  | input == ":help" || input == ":h" = do
      putStrLn "Pattern Lisp REPL"
      putStrLn "Commands: :quit, :q, :help, :h"
      return (env, True)
  | null (trim input) = return (env, True)
  | otherwise = case parseExpr input of
      Left err -> do
        hPutStrLn stderr (formatError err)
        return (env, True)
      Right expr -> case evalExprWithEnv expr env of
        Left evalErr -> do
          hPutStrLn stderr (formatError evalErr)
          return (env, True)
        Right (val, newEnv) -> do
          putStrLn (formatValue val)
          return (newEnv, True)
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- | Main REPL loop
repl :: Env -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  eof <- isEOF
  if eof
    then return ()
    else do
      input <- getLine
      (newEnv, shouldContinue) <- processLine input env
      if shouldContinue
        then repl newEnv
        else return ()

-- | Main entry point
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  repl initialEnv
