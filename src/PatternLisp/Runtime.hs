-- | Runtime system for Pattern Lisp tool execution and state management.
--
-- This module provides functions for validating tools (canonical form:
-- lambda with single "state" parameter), executing tools with Pattern Subject
-- state, and managing runtime state including execution traces.
--
-- Example usage:
--
-- > import PatternLisp.Runtime
-- > import PatternLisp.Parser
-- >
-- > case parseExpr "(lambda (state) state)" of
-- >   Right expr -> case validateTool expr initialEnv of
-- >     Right tool -> executeTool tool inputState env
-- >     Left err -> Left err
module PatternLisp.Runtime
  ( RuntimeState(..)
  , validateTool
  , executeTool
  ) where

import PatternLisp.Syntax
import PatternLisp.Eval
import Pattern (Pattern)
import Subject.Core (Subject)
import qualified Data.Map as Map

-- | Runtime state containing environment and execution trace.
--
-- The environment contains functions (from .plisp files) and state variables
-- (from .gram files). The execution trace records state transformations for
-- debugging and replay.
data RuntimeState = RuntimeState
  { environment :: Env  -- ^ Functions and states from files
  , executionTrace :: [(String, Pattern Subject, Pattern Subject)]  -- ^ Optional trace
  }
  deriving (Eq, Show)

-- | Validates that an expression is a tool in canonical form.
--
-- A valid tool must be:
-- 1. A lambda expression
-- 2. With exactly one parameter named "state"
-- 3. That evaluates to a closure
--
-- Returns the validated closure value, or an error if validation fails.
validateTool :: Expr -> Env -> Either Error Value
validateTool expr runtimeEnv = do
  -- Evaluate the expression to get a value
  val <- evalExpr expr runtimeEnv
  
  -- Check that it's a closure
  case val of
    VClosure (Closure paramNames _ _) -> do
      -- Check parameter count
      case paramNames of
        [paramName] -> do
          -- Check parameter name
          if paramName /= "state"
            then Left $ TypeMismatch
                   ("Tool parameter must be named 'state', but got '" ++ paramName ++ "'")
                   val
            else Right val
        _ -> Left $ TypeMismatch 
               ("Tool must have exactly one parameter, but got " ++ show (length paramNames) ++ " parameters")
               val
    _ -> Left $ TypeMismatch
           ("Tool must be a lambda expression, but got: " ++ show val)
           val

-- | Executes a tool with a Pattern Subject input state.
--
-- The tool must be a validated closure (from validateTool). The function:
-- 1. Applies the tool closure to the input state
-- 2. Verifies the result is a Pattern Subject
-- 3. Returns the transformed Pattern Subject
--
-- Returns an error if:
-- - Tool execution fails
-- - Tool returns a non-Pattern value
executeTool :: Value -> Pattern Subject -> Env -> Either Error (Pattern Subject)
executeTool toolVal inputState _env = do
  -- Verify tool is a closure
  case toolVal of
    VClosure closure -> do
      -- Apply closure to input state (wrapped as VPattern)
      let inputValue = VPattern inputState
      
      -- Use EvalM's applyClosure logic by running it in the environment
      -- We need to extract the closure application from Eval module
      -- Since applyClosure is not exported, we'll reimplement the logic
      let (Closure paramNames bodyExpr capturedEnv) = closure
      
      -- Check arity
      if length [inputValue] /= length paramNames
        then Left $ ArityMismatch "tool" (length paramNames) 1
        else do
          -- Extend captured environment with argument bindings
          let argBindings = Map.fromList (zip paramNames [inputValue])
          let extendedEnv = Map.union argBindings capturedEnv
          
          -- Evaluate body in extended environment
          result <- evalExpr bodyExpr extendedEnv
          
          -- Verify result is a Pattern
          case result of
            VPattern outputState -> Right outputState
            _ -> Left $ TypeMismatch
                   ("Tool must return a Pattern Subject, but returned: " ++ show result)
                   result
    _ -> Left $ TypeMismatch
           ("executeTool expects a closure, but got: " ++ show toolVal)
           toolVal

