{-|
Module      : Solutions.Hohmann
Description : Solutions for the Hohmann module.
-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeOperators    #-}
module Solutions.Hohmann where

import           Control.Lens       ((^.))
import           Data.LinearMap     ((:-*), linear)
import           Data.List.NonEmpty (NonEmpty)
import           Linear             (V2, norm, normalize, quadrance, (*^),
                                     (^+^), (^/), _x, _y)

import qualified Hohmann.Types      as T
import qualified ODE                as ODE


-- | Standard earth gravity.
g0 :: Double
g0 = 9.80665  -- m/s^2


-- | Find the craft angle (theta) from the state.
--
-- The angle should run from [0, 2*pi). Note that this range is different
-- from the normal atan2 function.
angle :: T.State -> Double
angle state =
  let
    p = T.position state
    q = atan2 (p^._y) (p^._x)
  in
    if q > 0
    then q
    else (2*pi) + q


-- | Find the gravitational force from the state.
gravity :: T.Params -> T.State -> V2 Double
gravity params state =
  let
    rMag2 = quadrance (T.position state)
    rHat = normalize (T.position state)
  in -(T.mu params) * (T.mass state) / rMag2 *^ rHat


-- | Find the thrust force from the state.
thrust :: T.Params -> T.State -> V2 Double
thrust params state =
  let vHat = normalize (T.velocity state)
  in g0 * (T.isp params) * (T.mDot params) *^ vHat


-- | Return @Nothing@ when @shouldTerminate == True@; @Just@ otherwise.
terminateWhen
  :: Bool      -- ^ when True, terminate
  -> a         -- ^ value to wrap
  -> Maybe a 
terminateWhen shouldTerminate input
  = if shouldTerminate
    then Nothing
    else Just input


-- | Compute a coasting trajectory.
--
-- The coasting trajectory is terminated once the craft reaches a given
-- termination condition.
coast
  :: T.Params                    -- ^ Simulation parameters.
  -> (T.State -> Bool)           -- ^ Termination condition (True to terminate).
  -> (Double, T.State)           -- ^ Initial time and state.
  -> NonEmpty (Double, T.State)  -- ^ Simulation steps.
coast params shouldTerminate s0 =
  let
    gradFn :: (Double, T.State) -> Maybe (Double :-* T.DState)
    gradFn (_, state)
      = terminateWhen (shouldTerminate state)
      $ linear
      $ \dt -> T.DState
               { T.dMass     = 0
               , T.dDistance = dt * norm (T.velocity state)
               , T.dPosition = dt *^ T.velocity state
               , T.dVelocity = dt *^ gravity params state ^/ T.mass state
               }

  in ODE.integrateTerminating ODE.rk4StepTerminating
                              (T.tEps params)
                              (T.tStep params)
                              s0
                              gradFn


-- | Compute a burn trajectory.
--
-- The burn trajectory is terminated once the craft reaches a given
-- velocity magnitude.
burn
  :: T.Params                    -- ^ Simulation parameters.
  -> Double                      -- ^ Termination velocity (m/s).
  -> (Double, T.State)           -- ^ Initial time and state.
  -> NonEmpty (Double, T.State)  -- ^ Simulation steps.
burn params velF s0 =
  let
    gradFn :: (Double, T.State) -> Maybe (Double :-* T.DState)
    gradFn (_, state)
      = terminateWhen (norm (T.velocity state) >= velF)
      $ linear
      $ \dt -> T.DState
               { T.dMass     = dt * -1 * T.mDot params
               , T.dDistance = dt * norm (T.velocity state)
               , T.dPosition = dt *^ T.velocity state
               , T.dVelocity = dt *^ (gravity params state ^+^ thrust params state) ^/ T.mass state
               }

  in ODE.integrateTerminating ODE.rk4StepTerminating
                              (T.tEps params)
                              (T.tStep params)
                              s0
                              gradFn
