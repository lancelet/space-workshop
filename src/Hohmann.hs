{-|
Module      : Hohmann
Description : Simulation of Hohmann transfer.
-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}  -- re-enable after completing solutions
module Hohmann where

import           Control.Lens       ((^.))
import           Data.LinearMap     ((:-*), linear)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Diagrams.Prelude   as D
import           Linear             (V2 (V2), norm, normalize, (*^), (^+^),
                                     (^/), _x, _y)

import qualified Hohmann.Types      as T
import qualified ODE                as ODE
import qualified Plot               as Plot
import qualified Solutions.Hohmann
import           Todo               (FallbackSolution (FallbackSolution), todo)


-- | Standard earth gravity.
g0 :: Double
g0 = 9.80665  -- m/s^2


-- | Find the craft angle (theta) from the state.
angle :: T.State -> Double
angle = todo (FallbackSolution Solutions.Hohmann.angle)


-- | Find the gravitational force from the state.
gravity :: T.Params -> T.State -> V2 Double
gravity = todo (FallbackSolution Solutions.Hohmann.gravity)


-- | Find the thrust force from the state.
thrust :: T.Params -> T.State -> V2 Double
thrust = todo (FallbackSolution Solutions.Hohmann.thrust)


-- | Return @Nothing@ when @shouldTerminate == True@; @Just@ otherwise.
terminateWhen
  :: Bool      -- ^ when True, terminate
  -> a         -- ^ value to wrap
  -> Maybe a
terminateWhen = todo (FallbackSolution Solutions.Hohmann.terminateWhen)


-- | Compute a coasting trajectory.
--
-- The coasting trajectory is terminated once the craft reaches a given
-- termination angle.
coast
  :: T.Params                    -- ^ Simulation parameters.
  -> (T.State -> Bool)           -- ^ Termination condition (True to terminate).
  -> (Double, T.State)           -- ^ Initial time and state.
  -> NonEmpty (Double, T.State)  -- ^ Simulation steps.
coast = todo (FallbackSolution Solutions.Hohmann.coast)


-- | Compute a burn trajectory.
--
-- The burn trajectory is terminated once the craft reaches a given
-- velocity magnitude.
burn
  :: T.Params                    -- ^ Simulation parameters.
  -> Double                      -- ^ Termination velocity (m/s).
  -> (Double, T.State)           -- ^ Initial time and state.
  -> NonEmpty (Double, T.State)  -- ^ Simulation steps.
burn = todo (FallbackSolution Solutions.Hohmann.burn)


-- | Plots a simulation of the Hohmann transfer with a normal, high-impulse
--   burn.
plotHighImpulseBurn :: Plot.Output -> IO ()
plotHighImpulseBurn output = plotBurn output 0.1 310 5.13


-- | Plots a simulation of the Hohmann transfer with a low-impulse burn.
plotLowImpulseBurn :: Plot.Output -> IO ()
plotLowImpulseBurn output = plotBurn output 2 200 0.1


-- | Plots a simulation of the Hohmann transfer with an ultra-low-impulse burn.
plotUltraLowImpulseBurn :: Plot.Output -> IO ()
plotUltraLowImpulseBurn output = plotBurn output 5 75 0.1


-- | General Hohmann transfer simulation.
plotBurn
  :: Plot.Output  -- ^ Plotting device for output.
  -> Double       -- ^ dt to use for the burn phase (s).
  -> Double       -- ^ specific impulse (s)
  -> Double       -- ^ mass flow rate (kg/s)
  -> IO ()
plotBurn output burndt isp mDot = do
  let
    -- Convert simulation result to a trajectory using km units.
    kmTrajectory :: NonEmpty (Double, T.State) -> [(Double, Double)]
    kmTrajectory = fmap (\(_, state) ->
                           let p = T.position state
                           in (p^._x / 1000.0, p^._y / 1000.0))
                   . NonEmpty.toList

    -- Simulation parameters.
    params :: T.Params
    params = T.Params
             { T.mu    = 4.905e12  -- value for the moon (m^3/s^2)
             , T.mDot  = mDot      -- kg/s
             , T.isp   = isp       -- Specific impulse (s)
             , T.r1    = 1780e3    -- inner radius (m) ; ~20 nautical miles
             , T.r2    = 1860e3    -- outer radius (m) ; ~60 nautical miles
             , T.tStep = 10        -- time step (s)
             , T.tEps  = 0.01      -- event accuracy (s)
             }

    -- Initial state at some radius.
    state0 :: Double -> T.State
    state0 radius = T.State
                    { T.mass     = 3000  -- kg
                    , T.distance = 0     -- m
                    , T.position = V2 radius 0
                    , T.velocity = V2 0 (sqrt(T.mu params / radius))
                    }

    -- Parameters for a burn.
    burnParams :: T.Params
    burnParams = params { T.tStep = burndt }

    -- Periapsis velocity of transfer trajectory. This is use as a terminal
    -- velocity for the first burn.
    vp = sqrt(T.mu burnParams * (2 / T.r1 burnParams - 2 / (T.r1 burnParams + T.r2 burnParams)))

    -- Velocity of final outer circular orbit. This is used as a terminal
    -- velocity for the second burn.
    vf = sqrt(T.mu params / T.r2 params)

    -- Trajectories.
    coastInner = coast params
                       -- termination is complicated because angles are periodic
                       (\state -> angle state >= 0
                                  && angle state < pi/2
                                  && T.distance state > 1e7)
                       (0, state0 (T.r1 params))
    burn1 = burn burnParams vp (0, state0 (T.r1 params))
    transferCoast = coast params (\state -> angle state >= pi) (NonEmpty.last burn1)
    burn2 = burn burnParams vf (NonEmpty.last transferCoast)
    coastOuter = coast params
                       -- termination is complicated because angles are periodic
                       (\state -> angle state >= pi/2
                                  && angle state < 3*pi/2
                                  && T.distance state > 2e7)
                       (NonEmpty.last burn2)

  Plot.plotOrbitSystem output
                       20.0
                       (Plot.OrbitSystem
                        { Plot.planet = (Plot.Planet "Moon" 1737.1 D.lightgray)
                        , Plot.systemItems =
                          [ Plot.Trajectory (kmTrajectory burn1) D.red
                          , Plot.Trajectory (kmTrajectory transferCoast) D.black
                          , Plot.Trajectory (kmTrajectory burn2) D.red
                          , Plot.Trajectory (kmTrajectory coastInner) D.skyblue
                          , Plot.Trajectory (kmTrajectory coastOuter) D.skyblue
                          , Plot.AltitudeCircle 1780 "40 km" D.gray
                          , Plot.AltitudeCircle 1820 "80 km" D.gray
                          , Plot.AltitudeCircle 1860 "120 km" D.gray
                          ]})
