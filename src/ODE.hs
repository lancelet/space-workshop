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
  , rk4Step
  , eulerStep
    -- ** Euler's method for Double only
  , integrateEulerDouble
  , eulerStepDouble
  ) where

import           Data.AffineSpace   (AffineSpace, Diff, (.+^))
import           Data.Basis         (Basis, HasBasis)
import           Data.LinearMap     ((:-*), lapply)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.MemoTrie      (HasTrie)
import           Data.VectorSpace   (Scalar, VectorSpace, (*^), (^+^), (^-^),
                                     (^/))

import qualified Solutions.ODE
import           Todo               (FallbackSolution (FallbackSolution), todo)


{------------------------------------------------------------------------------
Problem 1: Euler integration.

a) Implement Euler's method specialized to Double. The relevant
   equations are:

     tNext = t + dt

     xNext = x + dx
           = x + (dx/dt)*dt
           = x + dt * f (t, x)
-------------------------------------------------------------------------------}


-- | Single step of Euler integration (specialized to 'Double').
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
--   |   |             ----------- dt = 1 is the time step
--   |   ---- x = 4 after the time step
--   -------- t = 3 after the time step
-- @
--
eulerStepDouble
  :: Double                        -- ^ Step size @dt@
  -> ((Double, Double) -> Double)  -- ^ Gradient function @f (t, x)@
  -> (Double, Double)              -- ^ Time and state before the step @(t, x)@
  -> (Double, Double)              -- ^ Time and state after the step @(t, x)@
eulerStepDouble -- dt f q@(t, x)
  = todo (FallbackSolution Solutions.ODE.eulerStepDouble)


{------------------------------------------------------------------------------
b) Implement a driver for multiple steps of Euler integration.

The NonEmpty.scanl function is a convenient way to drive the
computation and accumulate results:

NonEmpty.scanl :: Foldable f => (b-> a -> b) -> b -> f a -> NonEmpty b
-------------------------------------------------------------------------------}


-- | Integrate an ODE using Euler's method (specialized to 'Double').
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


{------------------------------------------------------------------------------
c) Generalize the Euler step to the vector-space classes.
-------------------------------------------------------------------------------}


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
  => time                              -- ^ Step size @dt@
  -> ((time, state) -> time :-* diff)  -- ^ Gradient function @f (x, t)@
  -> (time, state)                     -- ^ Before the step @(t, x)@
  -> (time, state)                     -- ^ After the step @(t, x)@
eulerStep -- dt f (t, x)
  = todo (FallbackSolution Solutions.ODE.eulerStep)


-- | Stepper function used in ODE integration.
type Stepper time state
   = time
  -> ((time, state) -> time :-* Diff state)
  -> (time, state)
  -> (time, state)


{------------------------------------------------------------------------------
d) Generalize the integration driver to the vector-space classes.
-------------------------------------------------------------------------------}


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
  :: ( AffineSpace state
     , diff ~ Diff state, VectorSpace diff
     , HasBasis time, HasTrie (Basis time)
     , s ~ Scalar diff, s ~ Scalar time, Fractional s )
  => Stepper time state                 -- ^ Stepper function
  -> state                              -- ^ Initial state
  -> NonEmpty time                      -- ^ Evaluation times
  -> ((time, state) -> time :-* diff)   -- ^ Gradient function
  -> NonEmpty (time, state)             -- ^ Computed states
integrate -- stepper x0 (t0 :| ts) f
  = todo (FallbackSolution Solutions.ODE.integrate)


{------------------------------------------------------------------------------
Problem 2: Implement 4th-order Runge-Kutta Integration (RK4)

The equations for a single step of RK4 are as follows:

  k1 = dt * f (t, x)
  k2 = dt * f (t + 0.5*dt, x + 0.5*k1)
  k3 = dt * f (t + 0.5*dt, x + 0.5*k2)
  k4 = dt * f (t + dt, x + k3)

  dx = (1/6)*k1 + (1/3)*k2 + (1/3)*k3 + (1/6)*k4

  xNext = x + dx
  tNext = t + dt
-------------------------------------------------------------------------------}


-- | Single step of 4th-order Runge-Kutta integration.
rk4Step
  :: ( AffineSpace state
     , diff ~ Diff state, VectorSpace diff
     , HasBasis time, HasTrie (Basis time)
     , s ~ Scalar diff, s ~ Scalar time, Fractional s )
  => time                              -- ^ Step size @dt@
  -> ((time, state) -> time :-* diff)  -- ^ Gradient function @f (x, t)@
  -> (time, state)                     -- ^ Before the step @(t, x)@
  -> (time, state)                     -- ^ After the step @(t, x)@
rk4Step -- dt f q@(t, x)
  = todo (FallbackSolution Solutions.ODE.rk4Step)


{- $setup

>>> :set -XFlexibleContexts -XNegativeLiterals -XTypeFamilies
>>> import Data.LinearMap (linear)

-- >>> :set -XQuasiQuotes -XFlexibleContexts -XTypeFamilies
-- >>> import Data.Metrology.Vector ((%), (|*|))
-- >>> import Data.Units.SI.Parser (si)

-}
