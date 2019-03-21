{-|
Module      : Solutions.ODE
Description : Solutions for the 'ODE' module.
-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Solutions.ODE
  ( -- * Functions
    -- ** vector-space versions
    integrate
  , integrateWithDiff
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


-------------------------------------------------------------------------------
-- Integration of ODEs; generalized to work with vector-space classes
-------------------------------------------------------------------------------

-- | Integrate an ODE.
integrate
  :: forall state diff time s.
     ( AffineSpace state
     , diff ~ Diff state, VectorSpace diff
     , HasBasis time, HasTrie (Basis time)
     , s ~ Scalar diff, s ~ Scalar time, Fractional s )
  => Stepper time state                 -- ^ Stepper function
  -> state                              -- ^ Initial state
  -> NonEmpty time                      -- ^ Evaluation times
  -> ((time, state) -> time :-* diff)   -- ^ Gradient function
  -> NonEmpty (time, state)             -- ^ Computed states
integrate stepper x0 (t0 :| ts) f =
  let
    stepFn :: (time, state) -> time -> (time, state)
    stepFn q@(ti, _) tf = stepper (tf ^-^ ti) f q
  in
    NonEmpty.scanl stepFn (t0, x0) ts


-- | Integrate an ODE, recording the gradient.
integrateWithDiff
  :: forall state diff time s.
     ( AffineSpace state
     , diff ~ Diff state, VectorSpace diff
     , HasBasis time, HasTrie (Basis time)
     , s ~ Scalar diff, s ~ Scalar time, Fractional s )
  => Stepper time state                     -- ^ Stepper function
  -> state                                  -- ^ Initial state
  -> NonEmpty time                          -- ^ Evaluation times
  -> ((time, state) -> time :-* diff)       -- ^ Gradient function
  -> NonEmpty (time, state, time :-* diff)  -- ^ Computed states
integrateWithDiff stepper x0 (t0 :| ts) f =
  let
    stepFn
      :: (time, state, time :-* diff)
      -> time
      -> (time, state, time :-* diff)
    stepFn (ti, si, _) tf =
      let
        (t', state') = stepper (tf ^-^ ti) f (ti, si)
      in
        (t', state', f (ti, si))
  in
    NonEmpty.scanl stepFn (t0, x0, f (t0, x0)) ts


-- | Stepper function used in ODE integration.
type Stepper time state
   = time
  -> ((time, state) -> time :-* Diff state)
  -> (time, state)
  -> (time, state)


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
rk4Step dt f (t, x) =
  let
    o6 = 1/6
    o3 = 1/3
    tf = t ^+^ dt
    midt = tf ^/ 2

    k1 = lapply (f (t,    x          )) dt
    k2 = lapply (f (midt, x .+^ k1^/2)) dt
    k3 = lapply (f (midt, x .+^ k2^/2)) dt
    k4 = lapply (f (tf,   x .+^ k3   )) dt
    xf = x .+^ o6*^k1 .+^ o3*^k2 .+^ o3*^k3 .+^ o6*^k4
  in
    (tf, xf)


-- | Single step of Euler integration.
eulerStep
  :: ( AffineSpace state
     , diff ~ Diff state, VectorSpace diff
     , HasBasis time, HasTrie (Basis time)
     , s ~ Scalar diff, s ~ Scalar time )
  => time                              -- ^ Step size @dt@
  -> ((time, state) -> time :-* diff)  -- ^ Gradient function @f (x, t)@
  -> (time, state)                     -- ^ Before the step @(t, x)@
  -> (time, state)                     -- ^ After the step @(t, x)@
eulerStep dt f q@(t, x) = (t ^+^ dt, x .+^ lapply (f q) dt)


-------------------------------------------------------------------------------
-- Euler's method specialized to Double only
-------------------------------------------------------------------------------


-- | Integrate an ODE using Euler's method (specialized to 'Double').
integrateEulerDouble
  :: ((Double, Double) -> Double)  -- ^ Gradient function @f (t, x)@
  -> Double                        -- ^ Initial state @x0@
  -> NonEmpty Double               -- ^ NonEmpty of @t@ values
  -> NonEmpty (Double, Double)     -- ^ NonEmpty of @(t, x)@ values
integrateEulerDouble f x0 (t0 :| ts) =
  let
    stepFn :: (Double, Double) -> Double -> (Double, Double)
    stepFn q@(tStart, _) tEnd = eulerStepDouble (tEnd - tStart) f q
  in
    NonEmpty.scanl stepFn (t0, x0) ts


-- | Single step of Euler integration (specialized to 'Double').
eulerStepDouble
  :: Double                        -- ^ Step size
  -> ((Double, Double) -> Double)  -- ^ Gradient function @f@
  -> (Double, Double)              -- ^ State before the step @(x, y)@
  -> (Double, Double)              -- ^ State after the step @(x, y)@
eulerStepDouble dt f q@(t, x) = (t + dt, x + (dt*f q))
