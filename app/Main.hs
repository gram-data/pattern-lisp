module Main where

import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import PatternLisp.Syntax
import PatternLisp.FileLoader
import PatternLisp.Gram
import System.IO
import System.Environment
import System.Exit
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List (isPrefixOf, isSuffixOf, partition, elemIndex, sortOn)
import Data.Maybe (maybe)
import Control.Applicative ((<|>))

-- | Format a Value for display
formatValue :: Value -> String
formatValue (VNumber n) = show n
formatValue (VString s) = T.unpack s
formatValue (VBool True) = "#t"
formatValue (VBool False) = "#f"
formatValue (VList vals) = "(" ++ unwords (map formatValue vals) ++ ")"
formatValue (VPattern _) = "<pattern>"
formatValue (VClosure _) = "<closure>"
formatValue (VPrimitive _) = "<primitive>"

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
    else unlines $ map formatBinding $ sortByVarName $ Map.toList env
  where
    sortByVarName = sortOn fst  -- Sort by variable name (first element of tuple)
    formatBinding (name, val) = 
      "  " ++ name ++ " : " ++ formatValueType val

-- | Format an Error for display
formatError :: Error -> String
formatError (UndefinedVar name _) = "Error: Undefined variable: " ++ name
formatError (TypeMismatch msg _) = "Error: Type mismatch: " ++ msg
formatError (ArityMismatch name expected actual) = 
  "Error: Arity mismatch in " ++ name ++ ": expected " ++ show expected ++ 
  " arguments, got " ++ show actual
formatError (DivisionByZero _) = "Error: Division by zero"
formatError (ParseError msg) = "Parse error: " ++ msg

-- | Trim whitespace from both ends of a string
trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- | Check if a file is a .plisp file
isPlisp :: FilePath -> Bool
isPlisp = isSuffixOf ".plisp"

-- | Check if a file is a .gram file
isGram :: FilePath -> Bool
isGram = isSuffixOf ".gram"

-- | Separate files from flags in command-line arguments
-- Excludes the expression argument that comes after -e or --eval
parseArgs :: [String] -> ([FilePath], [String])
parseArgs args = 
  let excludeEvalExpr [] = ([], [])
      excludeEvalExpr [x] 
        | x `elem` ["-i", "--interactive", "-e", "--eval", "-h", "--help"] = ([], [x])
        | otherwise = ([x], [])
      excludeEvalExpr (x:y:xs)
        | x `elem` ["-e", "--eval"] = 
            let (files', flags') = excludeEvalExpr xs
            in (files', x : flags')  -- Include -e in flags, skip y (the expression)
        | x `elem` ["-i", "--interactive", "-h", "--help"] = 
            let (files', flags') = excludeEvalExpr (y:xs)
            in (files', x : flags')
        | otherwise = 
            let (files', flags') = excludeEvalExpr (y:xs)
            in (x : files', flags')
  in excludeEvalExpr args

-- | Extract eval expression from arguments (after -e or --eval)
extractEvalExpr :: [String] -> Maybe String
extractEvalExpr args = do
  idx <- elemIndex "-e" args <|> elemIndex "--eval" args
  if idx + 1 < length args
    then Just (args !! (idx + 1))
    else Nothing

-- | Check if -i or --interactive flag is present
hasInteractiveFlag :: [String] -> Bool
hasInteractiveFlag args = "-i" `elem` args || "--interactive" `elem` args

-- | Check if -e or --eval flag is present
hasEvalFlag :: [String] -> Bool
hasEvalFlag args = "-e" `elem` args || "--eval" `elem` args

-- | Check if -h or --help flag is present
hasHelpFlag :: [String] -> Bool
hasHelpFlag args = "-h" `elem` args || "--help" `elem` args

-- | Process a single REPL line
processLine :: String -> Env -> IO (Env, Bool)
processLine input env
  | input == ":quit" || input == ":q" = return (env, False)
  | input == ":help" || input == ":h" = do
      putStrLn "Pattern Lisp REPL"
      putStrLn "Commands:"
      putStrLn "  :quit, :q        Exit REPL"
      putStrLn "  :help, :h         Show this help"
      putStrLn "  :vars, :v        Show variables in scope"
      putStrLn ""
      putStrLn "  Execute tools: (tool-name state-name)"
      return (env, True)
  | input == ":vars" || input == ":v" = do
      putStrLn "Variables in scope:"
      putStrLn (formatEnv env)
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

-- | Execute expression in loaded environment
executeWithEval :: [String] -> String -> IO ()
executeWithEval allArgs exprStr = do
  -- Separate files from flags
  let (files, flags) = parseArgs allArgs
  
  -- Load files
  envResult <- processFiles files initialEnv
  case envResult of
    Left err -> do
      hPutStrLn stderr (formatError err)
      exitFailure
    Right env -> do
      -- Parse and evaluate expression
      case parseExpr exprStr of
        Left parseErr -> do
          hPutStrLn stderr (formatError parseErr)
          exitFailure
        Right expr -> do
          case evalExprWithEnv expr env of
            Left evalErr -> do
              hPutStrLn stderr (formatError evalErr)
              exitFailure
            Right (val, _) -> do
              -- If result is a Pattern, output as gram; otherwise format as value
              case val of
                VPattern pat -> putStr (patternToGram pat)
                _ -> putStrLn (formatValue val)

-- | Load files and output the last plisp file's result
executeFiles :: [FilePath] -> IO ()
executeFiles files = do
  envResult <- processFiles files initialEnv
  case envResult of
    Left err -> do
      hPutStrLn stderr (formatError err)
      exitFailure
    Right env -> do
      -- Get plisp files in order
      let plispFiles = filter isPlisp files
      case plispFiles of
        [] -> do
          -- No plisp files, just exit successfully (gram files were loaded)
          return ()
        _ -> do
          -- Get the last plisp file's result
          let lastPlispFile = last plispFiles
              name = deriveNameFromFilename lastPlispFile
          case Map.lookup name env of
            Nothing -> do
              hPutStrLn stderr $ "Error: Could not find result for file: " ++ name
              exitFailure
            Just val -> do
              -- If result is a Pattern, output as gram; otherwise format as value
              case val of
                VPattern pat -> putStr (patternToGram pat)
                _ -> putStrLn (formatValue val)

-- | Print usage message
usage :: IO ()
usage = do
  hPutStrLn stderr "Usage: pattern-lisp [OPTIONS] [FILES...]"
  hPutStrLn stderr ""
  hPutStrLn stderr "Modes:"
  hPutStrLn stderr "  No arguments: Start interactive REPL"
  hPutStrLn stderr "  Files only: Load files, evaluate last .plisp file, output result to stdout"
  hPutStrLn stderr "  Files + -e: Load files, evaluate expression, output result"
  hPutStrLn stderr "  Files + -i: Load files, then start interactive REPL"
  hPutStrLn stderr ""
  hPutStrLn stderr "Options:"
  hPutStrLn stderr "  -h, --help         Show this help message"
  hPutStrLn stderr "  -i, --interactive  Force interactive mode (load files, then REPL)"
  hPutStrLn stderr "  -e, --eval EXPR    Evaluate expression after loading files, then exit"
  hPutStrLn stderr ""
  hPutStrLn stderr "File types:"
  hPutStrLn stderr "  .plisp files: Evaluated as Pattern Lisp expressions"
  hPutStrLn stderr "  .gram files: Loaded as Pattern Subject values"
  hPutStrLn stderr ""
  hPutStrLn stderr "Examples:"
  hPutStrLn stderr "  pattern-lisp                                    # Interactive REPL"
  hPutStrLn stderr "  pattern-lisp script.plisp                       # Load script, eval, output result"
  hPutStrLn stderr "  pattern-lisp script.plisp state.gram            # Load both, eval script, output result"
  hPutStrLn stderr "  pattern-lisp script.plisp -i                   # Load script, then REPL"
  hPutStrLn stderr "  pattern-lisp script.plisp -e \"(+ 1 2)\"          # Load script, eval expr, output result"

-- | Main entry point
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- getArgs
  
  -- Check for help flag first
  if hasHelpFlag args
    then usage >> exitSuccess
    else do
      -- Separate files from flags
      let (files, flags) = parseArgs args
          plispFiles = filter isPlisp files
          gramFiles = filter isGram files
          hasInteractive = hasInteractiveFlag args
          hasEval = hasEvalFlag args
      
      case (files, hasInteractive, hasEval) of
        -- No files: Interactive REPL
        ([], False, False) -> repl initialEnv
        
        -- Both -i and -e specified: Error
        (_, True, True) -> do
          hPutStrLn stderr "Error: Cannot specify both -i and -e flags"
          usage
          exitFailure
        
        -- -i flag: Interactive mode (load files, then REPL)
        (_, True, False) -> do
          envResult <- processFiles files initialEnv
          case envResult of
            Left err -> hPutStrLn stderr (formatError err) >> exitFailure
            Right env -> repl env
        
        -- -e flag: Evaluate expression (after loading files)
        (_, _, True) -> do
          case extractEvalExpr args of
            Nothing -> do
              hPutStrLn stderr "Error: -e/--eval requires an expression argument"
              usage
              exitFailure
            Just exprStr -> executeWithEval args exprStr
        
        -- Files with no flags: Default behavior - load files, eval and exit
        (_, False, False) -> executeFiles files
