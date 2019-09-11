{-|
Module      : ODE
Description : Integration of Ordinary Differential Equations.

Solutions for the problems in this module are contained in the
'Solutions.ODE' module.
-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}  -- re-enable after completing solutions
module ODE
  ( -- * Types
    Stepper
    -- * Functions
    -- ** Useful utilities
  , linspace
    -- ** vector-space versions
  , integrate
  , integrateWithDiff
  , rk4Step
  , eulerStep
    -- ** self-terminating versions
  , integrateTerminating
  , rk4StepTerminating
    -- ** Euler's method for Double only
  , integrateEulerDouble
  , eulerStepDouble
  ) where

import           Data.AffineSpace   (AffineSpace, Diff, (.+^))
import           Data.Basis         (Basis, HasBasis)
import           Data.LinearMap     ((:-*), lapply)
import qualified Data.List          as List
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.MemoTrie      (HasTrie)
import           Data.VectorSpace   (Scalar, VectorSpace, (*^), (^+^), (^-^),
                                     (^/))

import qualified Solutions.ODE
import           Todo               (FallbackSolution (FallbackSolution), todo)


-- | Single step of Euler integration (specialized to 'Double').
--
-- Euler's method specialized to Double. The relevant equations are:
--
--   tNext = t + h
--
--   xNext = x + h
--         = x + (dx/dt)*h
--         = x + h * f (t, x)
--
-- Example:
--
-- >>> f (_, x) = -0.2 * x  -- this is an exponential decay equation
-- >>> eulerStepDouble 1 f (2, 5)
-- (3.0,4.0)
--
-- @
--   ^   ^             ^    ^  ^
--   |   |             |    |  |
--   |   |             |    |  --- x = 5 before the time step
--   |   |             |    ------ t = 2 before the time step
--   |   |             ----------- h = 1 is the time step
--   |   ---- x = 4 after the time step
--   -------- t = 3 after the time step
-- @
--
eulerStepDouble
  :: Double                        -- ^ Step size @h@
  -> ((Double, Double) -> Double)  -- ^ Gradient function @f (t, x)@
  -> (Double, Double)              -- ^ Time and state before the step @(t, x)@
  -> (Double, Double)              -- ^ Time and state after the step @(t, x)@
eulerStepDouble -- h f q@(t, x)
  = todo (FallbackSolution Solutions.ODE.eulerStepDouble)


-- | Integrate an ODE using Euler's method (specialized to 'Double').
--
-- NOTE: The NonEmpty.scanl function is a convenient way to drive the
--       computation.
--
-- Example:
--
-- >>> f (_, x) = -0.2 * x  -- this is an exponential decay equation
-- >>> times = NonEmpty.fromList [ 1, 2, 3 ]
-- >>> x0 = 50.0
-- >>> integrateEulerDouble f x0 times
-- (1.0,50.0) :| [(2.0,40.0),(3.0,32.0)]
integrateEulerDouble
  :: ((Double, Double) -> Double)  -- ^ Gradient function @f (t, x)@
  -> Double                        -- ^ Initial state @x0@
  -> NonEmpty Double               -- ^ NonEmpty of @t@ values
  -> NonEmpty (Double, Double)     -- ^ NonEmpty of @(t, x)@ values
integrateEulerDouble -- f x0 (t0 :| ts)
  = todo (FallbackSolution Solutions.ODE.integrateEulerDouble)
  {- Template:
  let
    stepFn :: (Double, Double) -> Double -> (Double, Double)
    stepFn q@(tStart, _) tEnd = -- TODO
  in
    NonEmpty.scanl stepFn (t0, x0) ts
  -}


-- | Linearly-spaced samples.
--
-- Example:
--
-- >>> linspace 5 0.0 10.0 :: [Double]
-- [0.0,2.0,4.0,6.0,8.0,10.0]
linspace
  :: ( VectorSpace a, s ~ Scalar a, Fractional s )
  => Int   -- ^ Number of divisions (1 less than the number of samples)
  -> a     -- ^ Start of the sample range (== first sample)
  -> a     -- ^ End of the sample range (== last sample for n > 2)
  -> [a]   -- ^ Samples
linspace n xStart xEnd =
  let
    n' = fromIntegral n
    range = xEnd ^-^ xStart
    f i = fromIntegral i *^ range ^/ n' ^+^ xStart
  in
    [ f i | i <- [0 .. n] ]


-- | Single step of Euler integration.
--
-- Example:
--
-- >>> f (_, x) = linear $ \dt -> -0.2 * x * dt
-- >>> eulerStep 1 f (2, 5)
-- (3.0,4.0)
eulerStep
  :: ( AffineSpace state
     , diff ~ Diff state, VectorSpace diff
     , HasBasis time, HasTrie (Basis time)
     , s ~ Scalar diff, s ~ Scalar time )
  => time                              -- ^ Step size @h@
  -> ((time, state) -> time :-* diff)  -- ^ Gradient function @f (x, t)@
  -> (time, state)                     -- ^ Before the step @(t, x)@
  -> (time, state)                     -- ^ After the step @(t, x)@
eulerStep -- h f (t, x)
  = todo (FallbackSolution Solutions.ODE.eulerStep)


-- | Stepper function used in ODE integration.
type Stepper time state
   = time
  -> ((time, state) -> time :-* Diff state)
  -> (time, state)
  -> (time, state)


-- | Integrate an ODE.
--
-- Example:
--
-- >>> f (_, x) = linear $ \dt -> -0.2 * x * dt
-- >>> times = NonEmpty.fromList [1, 2, 3]
-- >>> x0 = 50.0
-- >>> integrate eulerStep x0 times f
-- (1.0,50.0) :| [(2.0,40.0),(3.0,32.0)]
integrate
  :: ( diff ~ Diff state
     , HasBasis time )
  => Stepper time state                 -- ^ Stepper function
  -> state                              -- ^ Initial state
  -> NonEmpty time                      -- ^ Evaluation times
  -> ((time, state) -> time :-* diff)   -- ^ Gradient function
  -> NonEmpty (time, state)             -- ^ Computed states
integrate -- stepper x0 (t0 :| ts) f
  = todo (FallbackSolution Solutions.ODE.integrate)


-- | Integrate an ODE, recording the gradient.
integrateWithDiff
  :: forall state diff time.
     ( diff ~ Diff state
     , HasBasis time )
  => Stepper time state                     -- ^ Stepper function
  -> state                                  -- ^ Initial state
  -> NonEmpty time                          -- ^ Evaluation times
  -> ((time, state) -> time :-* diff)       -- ^ Gradient function
  -> NonEmpty (time, state, time :-* diff)  -- ^ Computed states
integrateWithDiff -- stepper x0 (t0 :| ts) f =
  = todo (FallbackSolution Solutions.ODE.integrateWithDiff)


-- | Single step of 4th-order Runge-Kutta integration.
--
--
-- The equations for a single step of RK4 are as follows:
--
--   k1 = h * f (t, x)
--   k2 = h * f (t + 0.5*h, x + 0.5*k1)
--   k3 = h * f (t + 0.5*h, x + 0.5*k2)
--   k4 = h * f (t + h, x + k3)
--
--   dx = (1/6)*k1 + (1/3)*k2 + (1/3)*k3 + (1/6)*k4
--
--   xNext = x + dx
--   tNext = t + h
rk4Step
  :: ( AffineSpace state
     , diff ~ Diff state, VectorSpace diff
     , HasBasis time, HasTrie (Basis time)
     , s ~ Scalar diff, s ~ Scalar time, Fractional s )
  => time                              -- ^ Step size @h@
  -> ((time, state) -> time :-* diff)  -- ^ Gradient function @f (x, t)@
  -> (time, state)                     -- ^ Before the step @(t, x)@
  -> (time, state)                     -- ^ After the step @(t, x)@
rk4Step -- h f q@(t, x)
  = todo (FallbackSolution Solutions.ODE.rk4Step)


-- | Stepper function for ODE integration that can terminate.
type TerminatingStepper time state
   = time
  -> ((time, state) -> Maybe (time :-* Diff state))
  -> (time, state)
  -> Maybe (time, state)


-- | Driver for an ODE that terminates itself.
--
-- Bisection is used to find a terminal value for the ODE at a time whose error
-- is less than @tEpsilon@.
integrateTerminating
  :: forall state diff time s.
     ( diff ~ Diff state
     , HasBasis time
     , s ~ Scalar time, Ord time, Fractional s )
  => TerminatingStepper time state             -- ^ Stepper to use.
  -> time                                      -- ^ Allowable error in the final time.
  -> time                                      -- ^ Step size @h@.
  -> (time, state)                             -- ^ Initial state.
  -> ((time, state) -> Maybe (time :-* diff))  -- ^ Gradient function.
  -> NonEmpty (time, state)                    -- ^ Computed states.
integrateTerminating {- stepper tEpsilon h state0 f -}
  = todo (FallbackSolution Solutions.ODE.integrateTerminating)


-- | Single step of terminating 4th-order Runge-Kutta integration.
--
-- A result is only returned if all none of the function evaluations indicate
-- termination.
rk4StepTerminating
  :: ( AffineSpace state
     , diff ~ Diff state, VectorSpace diff
     , HasBasis time, HasTrie (Basis time)
     , s ~ Scalar time, s ~ Scalar diff, Fractional s )
  => time                                            -- ^ Step size @h@
  -> ((time, state) -> Maybe (time :-* Diff state))  -- ^ Gradient function @f (x, t)@
  -> (time, state)                                   -- ^ Before the step @(t, x)@
  -> Maybe (time, state)                             -- ^ Optional @(t, x)@ after the step
rk4StepTerminating {- h f (t, x) -}
  = todo (FallbackSolution Solutions.ODE.rk4StepTerminating)


{- $setup

>>> :set -XFlexibleContexts -XNegativeLiterals -XTypeFamilies
>>> import Data.LinearMap (linear)

-- >>> :set -XQuasiQuotes -XFlexibleContexts -XTypeFamilies
-- >>> import Data.Metrology.Vector ((%), (|*|))
-- >>> import Data.Units.SI.Parser (si)

-}
