{-|
Module      : Todo
Description : Handle problem Todos.

This is a hacky solution to allow the workshop code to run,
unmodified, for the purpose of running tests (or to skip straight to
results), yet still allow participants to write code themselves.

If the environment variable @IDDQD@ is set then the 'todo' function
will use its "fallback solution". The lookup is done using an
'unsafePerformIO'.
-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Todo
  ( -- * Types
    FallbackSolution(..)
    -- * Functions
  , todo
  , getFallbackEnv
  ) where

import           GHC.Stack          (HasCallStack, callStack, getCallStack,
                                     srcLocFile, srcLocStartLine)
import           System.Environment (lookupEnv)
import           System.IO.Unsafe   (unsafePerformIO)


-- | A fallback solution.
data FallbackSolution a = FallbackSolution a


-- | Indicate a problem to be completed by a workshop participant.
todo :: (HasCallStack) => FallbackSolution a -> a
todo (FallbackSolution fallback) =
  case (unsafePerformIO getFallbackEnv) of
    UseFallbackSolution -> fallback
    FailNow ->
      error $ case (getCallStack callStack) of
        ((_, srcLoc):_) ->
          "Solution not implemented: File "
          <> srcLocFile srcLoc
          <> ", line "
          <> show (srcLocStartLine srcLoc)
          <> "."
        _ -> "Solution not implemented. Location unknown (sorry)."


-- | Indicate which solution to use.
data UseWhichSolution
  = UseFallbackSolution  -- ^ Use the fallback solution.
  | FailNow              -- ^ Fail right now.


-- | Read the environment to check if we should use fallback solutions.
--
-- If the variable @IDDQD@ is set then the fallback solutions will be
-- used.
getFallbackEnv :: IO UseWhichSolution
getFallbackEnv = lookupEnv "IDDQD" >>= \case
  Just _ -> pure UseFallbackSolution
  _      -> pure FailNow
