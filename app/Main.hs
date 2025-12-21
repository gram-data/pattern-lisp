module Main where

import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import PatternLisp.Syntax
import PatternLisp.FileLoader
import PatternLisp.Runtime
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
parseArgs :: [String] -> ([FilePath], [String])
parseArgs args = partition (\arg -> not (arg `elem` ["-i", "--interactive", "-e", "--eval"])) args

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

-- | Execute a tool on a state and output result as gram
executeToolOnState :: FilePath -> FilePath -> IO ()
executeToolOnState toolFile stateFile = do
  -- Load files
  envResult <- processFiles [stateFile, toolFile] initialEnv
  case envResult of
    Left err -> do
      hPutStrLn stderr (formatError err)
      exitFailure
    Right env -> do
      -- Get tool and state from environment
      let toolName = deriveNameFromFilename toolFile
          stateName = deriveNameFromFilename stateFile
      
      -- Get tool value
      case Map.lookup toolName env of
        Nothing -> do
          hPutStrLn stderr $ "Error: Tool not found: " ++ toolName
          exitFailure
        Just toolVal -> do
          -- Get state value
          case Map.lookup stateName env of
            Nothing -> do
              hPutStrLn stderr $ "Error: State not found: " ++ stateName
              exitFailure
            Just (VPattern inputState) -> do
              -- Execute tool
              case executeTool toolVal inputState env of
                Left execErr -> do
                  hPutStrLn stderr (formatError execErr)
                  exitFailure
                Right outputState -> do
                  -- Output as gram
                  putStr (patternToGram outputState)
            Just _ -> do
              hPutStrLn stderr $ "Error: State is not a Pattern: " ++ stateName
              exitFailure

-- | Execute a tool reading gram from stdin
executeToolOnStdin :: FilePath -> IO ()
executeToolOnStdin toolFile = do
  -- Read gram from stdin
  gramInput <- getContents
  case gramToPattern gramInput of
    Left parseErr -> do
      hPutStrLn stderr $ "Error parsing gram from stdin: " ++ show parseErr
      exitFailure
    Right inputState -> do
      -- Load tool
      envResult <- processFiles [toolFile] initialEnv
      case envResult of
        Left err -> do
          hPutStrLn stderr (formatError err)
          exitFailure
        Right env -> do
          -- Get tool from environment
          let toolName = deriveNameFromFilename toolFile
          case Map.lookup toolName env of
            Nothing -> do
              hPutStrLn stderr $ "Error: Tool not found: " ++ toolName
              exitFailure
            Just toolVal -> do
              -- Execute tool
              case executeTool toolVal inputState env of
                Left execErr -> do
                  hPutStrLn stderr (formatError execErr)
                  exitFailure
                Right outputState -> do
                  -- Output as gram
                  putStr (patternToGram outputState)

-- | Execute expression in loaded environment
executeWithEval :: [String] -> String -> IO ()
executeWithEval allArgs exprStr = do
  -- Separate files from flags
  let (files, flags) = parseArgs allArgs
      plispFiles = filter isPlisp files
      gramFiles = filter isGram files
  
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

-- | Print usage message
usage :: IO ()
usage = do
  hPutStrLn stderr "Usage: pattern-lisp [OPTIONS] [FILES...]"
  hPutStrLn stderr ""
  hPutStrLn stderr "Modes:"
  hPutStrLn stderr "  No arguments: Start interactive REPL"
  hPutStrLn stderr "  1 .plisp + 1 .gram: Auto-execute tool on state (output as gram)"
  hPutStrLn stderr "  1 .plisp (no .gram): Read gram from stdin, execute tool, output as gram"
  hPutStrLn stderr "  Multiple files: Must specify -i (interactive) or -e <expr> (evaluate)"
  hPutStrLn stderr ""
  hPutStrLn stderr "Options:"
  hPutStrLn stderr "  -i, --interactive  Force interactive mode (load files, then REPL)"
  hPutStrLn stderr "  -e, --eval EXPR    Evaluate expression and exit"
  hPutStrLn stderr ""
  hPutStrLn stderr "Examples:"
  hPutStrLn stderr "  pattern-lisp                                    # Interactive REPL"
  hPutStrLn stderr "  pattern-lisp tool.plisp state.gram              # Execute tool on state"
  hPutStrLn stderr "  pattern-lisp tool.plisp < input.gram > output.gram  # Pipe gram through tool"
  hPutStrLn stderr "  pattern-lisp tool1.plisp tool2.plisp state.gram -i  # Interactive with files"
  hPutStrLn stderr "  pattern-lisp tool1.plisp state.gram -e \"(tool1 state)\"  # Explicit execution"

-- | Main entry point
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- getArgs
  
  -- Separate files from flags
  let (files, flags) = parseArgs args
      plispFiles = filter isPlisp files
      gramFiles = filter isGram files
      hasInteractive = hasInteractiveFlag args
      hasEval = hasEvalFlag args
  
  case (files, hasInteractive, hasEval) of
    -- No files: Interactive REPL
    ([], False, False) -> repl initialEnv
    
    -- Single tool + single state: Auto-execute (unless -i)
    ([toolFile, stateFile], False, False) | isPlisp toolFile && isGram stateFile ->
      if hasInteractive
        then do
          envResult <- processFiles [stateFile, toolFile] initialEnv
          case envResult of
            Left err -> hPutStrLn stderr (formatError err) >> exitFailure
            Right env -> repl env
        else executeToolOnState toolFile stateFile
    
    -- Single tool, no state: Read from stdin (unless -i)
    ([toolFile], False, False) | isPlisp toolFile ->
      if hasInteractive
        then do
          envResult <- processFiles [toolFile] initialEnv
          case envResult of
            Left err -> hPutStrLn stderr (formatError err) >> exitFailure
            Right env -> repl env
        else executeToolOnStdin toolFile
    
    -- Multiple files: Must specify -i OR -e
    (_, False, False) | length files > 1 -> do
      hPutStrLn stderr "Error: Multiple files require -i (interactive) or -e <expr> (evaluate)"
      usage
      exitFailure
    
    -- Both -i and -e specified: Error
    (_, True, True) -> do
      hPutStrLn stderr "Error: Cannot specify both -i and -e flags"
      usage
      exitFailure
    
    -- -i flag: Interactive mode
    (_, True, False) -> do
      envResult <- processFiles files initialEnv
      case envResult of
        Left err -> hPutStrLn stderr (formatError err) >> exitFailure
        Right env -> repl env
    
    -- -e flag: Evaluate expression
    (_, False, True) -> do
      case extractEvalExpr args of
        Nothing -> do
          hPutStrLn stderr "Error: -e/--eval requires an expression argument"
          usage
          exitFailure
        Just exprStr -> executeWithEval args exprStr
    
    -- Default: Show usage
    _ -> do
      usage
      exitFailure
