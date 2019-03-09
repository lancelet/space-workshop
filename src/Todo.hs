{-|
Module      : Todo
Description : Handle problem Todos.
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
--   If the variable @SPACE_WORKSHOP_FALLBACKS@ is set then the fallback
--   solutions will be used.
getFallbackEnv :: IO UseWhichSolution
getFallbackEnv = lookupEnv "SPACE_WORKSHOP_FALLBACKS" >>= \case
  Just _ -> pure UseFallbackSolution
  _      -> pure FailNow
