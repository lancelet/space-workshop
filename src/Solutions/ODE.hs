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
    -- ** terminating versions
    integrateTerminating
  , rk4StepTerminating
    -- ** vector-space versions
  , integrate
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
import qualified Data.List          as List
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.MemoTrie      (HasTrie)
import           Data.VectorSpace   (Scalar, VectorSpace, (*^), (^+^), (^-^),
                                     (^/))

-------------------------------------------------------------------------------
-- Integration of ODEs with termination
-------------------------------------------------------------------------------

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
integrateTerminating stepper tEpsilon h state0 f =
  let

    -- Use a list internally, to avoid duplicating starting elements
    -- if/when we halve the time step.
    listIntegrator
      :: time             -- ^ dt
      -> (time, state)    -- ^ initial state
      -> [(time, state)]  -- ^ list of states produced
    listIntegrator dt' state0' =
      let
        stepFn :: (time, state) -> Maybe ((time, state), (time, state))
        stepFn timeState = stepper dt' f timeState >>= \r -> pure (r, r)

        unfold1 :: [(time, state)]
        unfold1 = List.unfoldr stepFn state0'
      in
        -- If our time step is less than the required error, then we
        -- know that our end point is definitely within tEpsilon and
        -- we can terminate. Otherwise we have to get closer to the end
        -- point, which we can do by halving the time step and acquiring
        -- new samples if necessary.
        if dt' < tEpsilon
        then unfold1
        else
          let
            -- We may not have generated any samples at all so far; so we
            -- need to handle that case by starting from the initial state.
            state0'' = case unfold1 of
                         [] -> state0'
                         _  -> last unfold1
          in
            unfold1 ++ listIntegrator (dt'^/2) state0''

  in state0 :| listIntegrator h state0


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
rk4StepTerminating h f (t, x) =
  let
    o6 = 1/6
    o3 = 1/3
    tf = t ^+^ h
    midt = t ^+^ (h ^/ 2)
    ldx dxdt = lapply dxdt h
  in do
    k1 <- ldx <$> f(t,    x          )
    k2 <- ldx <$> f(midt, x .+^ k1^/2)
    k3 <- ldx <$> f(midt, x .+^ k2^/2)
    k4 <- ldx <$> f(tf,   x .+^ k3   )
    let xf = x .+^ o6*^k1 .+^ o3*^k2 .+^ o3*^k3 .+^ o6*^k4
    pure (tf, xf)


-------------------------------------------------------------------------------
-- Integration of ODEs; generalized to work with vector-space classes
-------------------------------------------------------------------------------

-- | Integrate an ODE.
integrate
  :: forall state diff time.
     ( diff ~ Diff state
     , HasBasis time )
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
  :: forall state diff time.
     ( diff ~ Diff state
     , HasBasis time )
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
  => time                              -- ^ Step size @h@
  -> ((time, state) -> time :-* diff)  -- ^ Gradient function @f (x, t)@
  -> (time, state)                     -- ^ Before the step @(t, x)@
  -> (time, state)                     -- ^ After the step @(t, x)@
rk4Step h f (t, x) =
  let
    o6 = 1/6
    o3 = 1/3
    tf = t ^+^ h
    midt = t ^+^ (h ^/ 2)

    k1 = lapply (f (t,    x          )) h
    k2 = lapply (f (midt, x .+^ k1^/2)) h
    k3 = lapply (f (midt, x .+^ k2^/2)) h
    k4 = lapply (f (tf,   x .+^ k3   )) h
    xf = x .+^ o6*^k1 .+^ o3*^k2 .+^ o3*^k3 .+^ o6*^k4
  in
    (tf, xf)


-- | Single step of Euler integration.
eulerStep
  :: ( AffineSpace state
     , diff ~ Diff state, VectorSpace diff
     , HasBasis time, HasTrie (Basis time)
     , s ~ Scalar diff, s ~ Scalar time )
  => time                              -- ^ Step size @h@
  -> ((time, state) -> time :-* diff)  -- ^ Gradient function @f (x, t)@
  -> (time, state)                     -- ^ Before the step @(t, x)@
  -> (time, state)                     -- ^ After the step @(t, x)@
eulerStep h f q@(t, x) = (t ^+^ h, x .+^ lapply (f q) h)


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
eulerStepDouble h f q@(t, x) = (t + h, x + (h*f q))
