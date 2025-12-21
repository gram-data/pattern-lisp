-- | File loading for Pattern Lisp with gram support.
--
-- This module provides functions to load `.plisp` and `.gram` files:
-- * `.plisp` files are parsed and evaluated as arbitrary Pattern Lisp expressions
-- * `.gram` files are loaded as Pattern Subject state variables
-- * Files are processed in order: `.gram` files first, then `.plisp` files
-- * Results are bound to filename-derived names in the environment
--
-- Example usage:
--
-- > import PatternLisp.FileLoader
-- >
-- > env <- processFiles ["state.gram", "script.plisp"] initialEnv
-- > case env of
-- >   Left err -> print err
-- >   Right env' -> use env'
module PatternLisp.FileLoader
  ( loadPlispFile
  , loadGramFile
  , processFiles
  , deriveNameFromFilename
  , FileLoadResult(..)
  ) where

import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Gram
import System.FilePath (takeBaseName)
import qualified Data.Map as Map
import Data.List (isSuffixOf, isPrefixOf, partition, elemIndex)
import Control.Monad (foldM)

-- | Result of loading a plisp file
data FileLoadResult = FileLoadResult
  { loadResultEnv :: Env      -- Updated environment with all define bindings
  , loadResultName :: String   -- Filename-derived name
  , loadResultValue :: Value   -- Final evaluated value
  }

-- | Derives a variable/function name from a filename.
-- Removes the extension and sanitizes the name.
deriveNameFromFilename :: FilePath -> String
deriveNameFromFilename = takeBaseName

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

-- | Loads a `.plisp` file and evaluates it.
--
-- The file content is parsed and evaluated. Multiple expressions are
-- wrapped in a `begin` form. All bindings from `define` statements are
-- preserved in the returned environment. The final result value is also
-- bound to the filename-derived name.
loadPlispFile :: FilePath -> Env -> IO (Either Error FileLoadResult)
loadPlispFile filepath runtimeEnv = do
  content <- readFile filepath
  case parseFileContent content of
    Left parseErr -> return $ Left parseErr
    Right expr -> do
      case evalExprWithEnv expr runtimeEnv of
        Left evalErr -> return $ Left evalErr
        Right (val, updatedEnv) -> do
          let name = deriveNameFromFilename filepath
              -- Merge the updated environment (with all defines) and add the final result
              finalEnv = Map.insert name val updatedEnv
          return $ Right $ FileLoadResult finalEnv name val

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
        Right fileResult -> return $ Right $ loadResultEnv fileResult

