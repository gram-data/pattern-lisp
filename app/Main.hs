module Main where

import Lisp.Parser
import Lisp.Eval
import Lisp.Primitives
import Lisp.Syntax
import System.IO
import System.Environment
import System.Exit
import qualified Data.Text as T
import Data.List (isPrefixOf)

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

-- | Evaluate a multi-line program (wraps in begin form)
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

-- | Load and evaluate a file
loadFile :: FilePath -> Env -> IO (Env, Bool)
loadFile filepath env = do
  content <- readFile filepath
  case evaluateProgram content env of
    Left err -> do
      hPutStrLn stderr (formatError err)
      return (env, True)
    Right (val, newEnv) -> do
      putStrLn (formatValue val)
      return (newEnv, True)

-- | Process a single REPL line
processLine :: String -> Env -> IO (Env, Bool)
processLine input env
  | input == ":quit" || input == ":q" = return (env, False)
  | input == ":help" || input == ":h" = do
      putStrLn "Pattern Lisp REPL"
      putStrLn "Commands: :quit, :q, :help, :h, :load <file>"
      return (env, True)
  | ":load" `isPrefixOf` input || ":l" `isPrefixOf` input = do
      let filepath = trim (dropWhile (/= ' ') (dropWhile (== ' ') input))
      if null filepath
        then do
          hPutStrLn stderr "Error: :load requires a file path"
          return (env, True)
        else loadFile filepath env
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
  args <- getArgs
  case args of
    [] -> repl initialEnv  -- Interactive REPL
    [filepath] -> do
      -- Load and execute file, then exit
      content <- readFile filepath
      case evaluateProgram content initialEnv of
        Left err -> do
          hPutStrLn stderr (formatError err)
          exitFailure
        Right (val, _) -> do
          putStrLn (formatValue val)
          return ()
    _ -> do
      hPutStrLn stderr "Usage: pattern-lisp [file.plisp]"
      hPutStrLn stderr "  No arguments: Start interactive REPL"
      hPutStrLn stderr "  One argument: Execute file and print result"
      exitFailure
