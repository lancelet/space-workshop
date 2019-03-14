{-|
Module      : ODE
Description : Integration of Ordinary Differential Equations.

Solutions for the problems in this module are contained in the
'Solutions.ODE' module.
-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}  -- re-enable after completing solutions
module ODE where

import           Control.Lens       (makeLenses, view, (^.), _1, _2)
import           Data.AdditiveGroup (AdditiveGroup)
import           Data.AffineSpace   (AffineSpace, Diff, (.+^), (.-.))
import           Data.Basis         (Basis, HasBasis)
import           Data.LinearMap     ((:-*), lapply)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.MemoTrie      (HasTrie)
import           Data.VectorSpace   (Scalar, VectorSpace, (*^))
import           GHC.Generics       (Generic)

import qualified Plot
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
-- >>> linspace 6 0.0 10.0
-- [0.0,2.0,4.0,6.0,8.0,10.0]
linspace
  :: Fractional a
  => Int   -- ^ Number of samples.
  -> a     -- ^ Start of the sample range (== first sample).
  -> a     -- ^ End of the sample range (== last sample for n > 2).
  -> [a]   -- ^ Samples.
linspace n xStart xEnd =
  let
    m = n - 1
    m' = fromIntegral m
    range = xEnd - xStart
    f i = fromIntegral i * range / m' + xStart
  in
    [ f i | i <- [0 .. m] ]


{------------------------------------------------------------------------------
c) Generalize the Euler step to the vector-space classes.
-------------------------------------------------------------------------------}


-- | Single step of Euler integration.
--
-- Example:
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
  -> ((time, state) -> time :-* (Diff state))
  -> (time, state)
  -> (time, state)
{-
   = s                -- ^ Step size @dt@.
  -> ((s, as) -> vs)  -- ^ Gradient function @f (x, t)@.
  -> (s, as)          -- ^ Time and state before the step @(t, x)@
  -> (s, as)          -- ^ Time and state after the step @(t, x)@
-}


{------------------------------------------------------------------------------
d) Generalize the integration driver to the vector-space classes.
-------------------------------------------------------------------------------}


-- | Integrate an ODE.
--
-- Example:
integrate
  :: ( AffineSpace state
     , diff ~ Diff state, VectorSpace diff
     , HasBasis time, HasTrie (Basis time)
     , s ~ Scalar diff, s ~ Scalar time )
  => Stepper time state
  -> state
  -> NonEmpty time
  -> ((time, state) -> time :-* diff)
  -> NonEmpty (time, state)
integrate = undefined
{-
integrate
  :: forall as vs s.
     ( AffineSpace as
     , vs ~ Diff as, VectorSpace vs
     , s ~ Scalar vs, Fractional s )
  => Stepper s as vs    -- ^ Stepper function
  -> as                 -- ^ Initial state @x0@
  -> NonEmpty s         -- ^ NonEmpty of @t@ values
  -> ((s, as) -> vs)    -- ^ Gradient function @f (t, x)@
  -> NonEmpty (s, as)   -- ^ NonEmpty of @(t, x)@ values
integrate -- stepper x0 ts f
  v todo (FallbackSolution Solutions.ODE.integrate)
-}


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
