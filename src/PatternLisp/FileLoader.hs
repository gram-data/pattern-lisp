-- | File loading for Pattern Lisp with implicit lambda wrapping and gram support.
--
-- This module provides functions to load `.plisp` and `.gram` files according to
-- the convention-based approach:
-- * `.plisp` files are automatically wrapped in `(lambda (state) ...)` if needed
-- * `.gram` files are loaded as Pattern Subject state variables
-- * Files are processed in order: `.gram` files first, then `.plisp` files
--
-- Example usage:
--
-- > import PatternLisp.FileLoader
-- >
-- > env <- processFiles ["state.gram", "tool.plisp"] initialEnv
-- > case env of
-- >   Left err -> print err
-- >   Right env' -> use env'
module PatternLisp.FileLoader
  ( loadPlispFile
  , loadGramFile
  , processFiles
  , deriveNameFromFilename
  ) where

import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Gram
import System.FilePath (takeBaseName)
import qualified Data.Map as Map
import Data.List (isSuffixOf, isPrefixOf, partition, elemIndex)
import Control.Monad (foldM)

-- | Derives a variable/function name from a filename.
-- Removes the extension and sanitizes the name.
deriveNameFromFilename :: FilePath -> String
deriveNameFromFilename = takeBaseName

-- | Checks if an expression is already a lambda with "state" parameter.
isStateLambda :: Expr -> Bool
isStateLambda (List [Atom (Symbol "lambda"), List [Atom (Symbol "state")], _]) = True
isStateLambda _ = False

-- | Strips inline comments (starting with ;) from a line
stripInlineComment :: String -> String
stripInlineComment line = 
  case elemIndex ';' line of
    Nothing -> line
    Just idx -> 
      -- Check if it's a comment (not inside a string)
      -- Simple heuristic: if ; appears, take everything before it
      -- This doesn't handle strings perfectly, but works for most cases
      let beforeSemicolon = take idx line
          trimmed = reverse $ dropWhile (== ' ') $ reverse beforeSemicolon
      in trimmed

-- | Parses a file that may contain multiple expressions.
-- If multiple expressions are found, wraps them in a `begin` form.
parseFileContent :: String -> Either Error Expr
parseFileContent content = do
  -- Try parsing as a single expression first
  case parseExpr content of
    Right expr -> Right expr
    Left _ -> do
      -- If that fails, try wrapping in begin form to handle multiple expressions
      -- Remove comment lines, empty lines, and strip inline comments
      let processedLines = map stripInlineComment $ 
                           filter (not . isCommentOrEmpty) (lines content)
          cleaned = unwords processedLines
          beginProgram = "(begin " ++ cleaned ++ ")"
      parseExpr beginProgram
  where
    isCommentOrEmpty line = 
      let trimmed = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse $ line
      in null trimmed || ";;" `isPrefixOf` trimmed

-- | Wraps a sequence of expressions in a lambda with "state" parameter.
-- The last expression becomes the return value.
wrapInStateLambda :: Expr -> Expr
wrapInStateLambda expr = 
  List [Atom (Symbol "lambda"), List [Atom (Symbol "state")], expr]

-- | Loads a `.plisp` file and wraps it in `(lambda (state) ...)` if needed.
--
-- If the file already contains a lambda with "state" parameter, it's used as-is.
-- Otherwise, the file content is wrapped in `(lambda (state) ...)`.
-- Multiple expressions in the file are wrapped in a `begin` form first.
-- The function evaluates to a closure and is bound to the filename-derived name.
loadPlispFile :: FilePath -> Env -> IO (Either Error (String, Value))
loadPlispFile filepath runtimeEnv = do
  content <- readFile filepath
  case parseFileContent content of
    Left parseErr -> return $ Left parseErr
    Right expr -> do
      -- Check if already a lambda with "state" parameter
      let wrappedExpr = if isStateLambda expr
                        then expr
                        else wrapInStateLambda expr
      
      case evalExpr wrappedExpr runtimeEnv of
        Left evalErr -> return $ Left evalErr
        Right val -> do
          -- Validate it's a tool (lambda with "state" parameter)
          case val of
            VClosure (Closure [paramName] _ _) | paramName == "state" -> do
              let name = deriveNameFromFilename filepath
              return $ Right (name, val)
            _ -> return $ Left $ TypeMismatch
                   ("File does not evaluate to a tool (lambda (state) ...): " ++ filepath)
                   val

-- | Loads a `.gram` file and parses it to Pattern Subject.
--
-- The file is parsed as gram notation and converted to Pattern Subject.
-- It's bound to the filename-derived name.
loadGramFile :: FilePath -> Env -> IO (Either Error (String, Value))
loadGramFile filepath _env = do
  content <- readFile filepath
  case gramToPattern content of
    Left parseErr -> return $ Left $ ParseError (show parseErr)
    Right pat -> do
      let name = deriveNameFromFilename filepath
      return $ Right (name, VPattern pat)

-- | Processes a list of files, loading them into the environment.
--
-- Files are processed in order:
-- 1. `.gram` files first (state variables)
-- 2. `.plisp` files second (functions that may use states)
--
-- Returns the updated environment with all bindings.
processFiles :: [FilePath] -> Env -> IO (Either Error Env)
processFiles files initialEnv = do
  let (gramFiles, plispFiles) = partition (isSuffixOf ".gram") files
  
  -- Process gram files first
  gramEnvResult <- foldM loadGramFileIntoEnv (Right initialEnv) gramFiles
  case gramEnvResult of
    Left err -> return $ Left err
    Right gramEnv -> do
      -- Process plisp files second
      foldM loadPlispFileIntoEnv (Right gramEnv) plispFiles
  where
    loadGramFileIntoEnv :: Either Error Env -> FilePath -> IO (Either Error Env)
    loadGramFileIntoEnv (Left loadErr) _ = return $ Left loadErr
    loadGramFileIntoEnv (Right currentEnv) filepath = do
      result <- loadGramFile filepath currentEnv
      case result of
        Left fileErr -> return $ Left fileErr
        Right (name, val) -> return $ Right $ Map.insert name val currentEnv
    
    loadPlispFileIntoEnv :: Either Error Env -> FilePath -> IO (Either Error Env)
    loadPlispFileIntoEnv (Left loadErr) _ = return $ Left loadErr
    loadPlispFileIntoEnv (Right currentEnv) filepath = do
      result <- loadPlispFile filepath currentEnv
      case result of
        Left fileErr -> return $ Left fileErr
        Right (name, val) -> return $ Right $ Map.insert name val currentEnv

