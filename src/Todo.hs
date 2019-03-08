{-|
Module      : Todo
Description : Handle problem Todos.

In the workshop, we want to:

  * Leave some functions for participants to fill out, and
  * Use fallbacks for those functions for testing purposes and so participants
    can see "correct" output if they want.

This module defines a 'todo' function which can appear in any definition, and
provides a fallback function option. Control of this is handled by exceptions
(yuck, but... it means we don't have to change type signatures). Then, on the
"driver" side, there's a function called 'unTodo', which catches the exception
thrown if a function is not implemented and can invoke the fallback if
necessary.
-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Todo
  ( -- * Types
    FallbackSolution(..)
  , UseWhichSolution
    -- * Functions
  , todo
  , unTodo
  , getFallbackEnv
  ) where

import           Control.Exception  (Exception, throw, tryJust)
import           Data.Typeable      (Typeable)
import           GHC.Stack          (CallStack, HasCallStack, callStack,
                                     getCallStack, srcLocFile, srcLocStartLine)
import           System.Environment (lookupEnv)


-- | A fallback solution.
data FallbackSolution a = FallbackSolution a


-- | Indicate a problem to be completed by a workshop participant.
--
--   This function throws a 'SolutionNotDoneError', pointing to a fallback
--   solution.
--
--   This is a stupid hack to avoid changing a signature from @a@ to something
--   more useful. It would be possible to return a data type instead of this
--   kludge, and avoid the naughty control passing via exceptions, but that
--   would make the type signatures of problems for the workshop (even) more
--   complicated.
todo :: (HasCallStack, Typeable a) => FallbackSolution a -> a
todo fallback = throw (SolutionNotDoneError callStack fallback)


-- | A nasty control-passing exception, used to indicate that a problem has not
--   been completed.
--
--   I would not recommend doing this in "real" code, ever.
data SolutionNotDoneError a
  = SolutionNotDoneError CallStack (FallbackSolution a)

instance Show (SolutionNotDoneError a) where
  show (SolutionNotDoneError stk _)
    = case getCallStack stk of
        [] ->
          "Solution not implemented. Location unknown (sorry)."
        ((fnName, srcLoc):_) ->
          "Solution not implemented: Function "
          <> fnName
          <> " in file "
          <> srcLocFile srcLoc
          <> ", line "
          <> show (srcLocStartLine srcLoc)
          <> "."

instance (Typeable a) => Exception (SolutionNotDoneError a)


-- | Indicate which solution to use.
data UseWhichSolution
  = UseFallbackSolution  -- ^ Use the fallback solution.
  | FailNow              -- ^ Fail right now.


-- | Catches a `SolutionNotDoneError`, using the fallback value if that option
--   is selected. The returned exception is converted to a String message.
unTodo
  :: forall a.
     ( Typeable a )
  => UseWhichSolution -- ^ Indicates whether the fallback function should be
                      --   used.
  -> a                -- ^ The value that may throw a `SolutionNotDoneError`.
  -> IO (Either String a)
unTodo whichSolution x = do
  attempt <- tryJust (\(e :: SolutionNotDoneError a) -> Just e) (pure x)
  pure $ case attempt of
    Right result -> Right result
    Left err ->
      case whichSolution of
        FailNow -> Left (show err)
        UseFallbackSolution ->
          let SolutionNotDoneError _ (FallbackSolution fallback) = err
          in Right fallback


-- | Read the environment to check if we should use fallback solutions.
--
--   If the variable @SPACE_WORKSHOP_FALLBACKS@ is set then the fallback
--   solutions will be used.
getFallbackEnv :: IO UseWhichSolution
getFallbackEnv = lookupEnv "SPACE_WORKSHOP_FALLBACKS" >>= \case
  Just _ -> pure UseFallbackSolution
  _      -> pure FailNow
