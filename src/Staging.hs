{-|
Module      : Staging
Description : Simulating staging of rockets.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- re-enable after completing solutions
module Staging where

import           Data.LinearMap     ((:-*), linear)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.VectorSpace   ((^*))

import qualified ODE
import qualified Plot
import qualified Solutions.Staging
import qualified Staging.Types      as T
import           Todo               (FallbackSolution (FallbackSolution), todo)


-- | Equation of motion for a rocket stage.
--
-- The rocket engine is burning in a vacuum in the absence of gravity.
equationOfMotion
  :: Double               -- ^ Mass flow rate (kg / s).
  -> Double               -- ^ Specific impulse (s).
  -> Double               -- ^ Dry mass of this stage (kg).
  -> Double               -- ^ Total mass of remaining stages (kg).
  -> (Double, T.State)    -- ^ Current time and state (s, State).
  -> Double :-* T.DState  -- ^ Linear map of derivatives.
equationOfMotion {- mDot isp mDry mRemaining (_, state) -}
  = todo (FallbackSolution Solutions.Staging.equationOfMotion)
  {-
  = linear ((^*) grad)
  where
    -- grad is a DState for a unit time increment
    grad :: T.DState
    grad = T.DState
           { T.dPropellantMass = undefined
           , T.dPosition       = undefined
           , T.dVelocity       = undefined
           }

    g0 :: Double  -- m/s^2
    g0 = 9.80665
  -}


-- | Burn a stage of a rocket (can be the first or second stage).
burnStage
  :: Double                      -- ^ Initial propellant mass (kg).
  -> Double                      -- ^ Dry mass of the current stage (kg).
  -> Double                      -- ^ Total mass of remaining mass of stages (kg).
  -> Int                         -- ^ Number of integration steps.
  -> Double                      -- ^ Initial time (s).
  -> T.State                     -- ^ Initial state.
  -> NonEmpty (Double, T.State)  -- ^ List of time and state tuples.
burnStage {- mp0 mDry mRemaining nSteps time0 state0 -}
  = todo (FallbackSolution Solutions.Staging.burnStage)
  {-
  where
    -- Duration of the burn (s).
    burnDuration :: Double
    burnDuration = undefined

    -- Evaluation times (s).
    times :: NonEmpty Double
    times = NonEmpty.fromList
            $ ODE.linspace nSteps time0 (time0 + burnDuration)

    -- Gradient function for ODE solver.
    gradFn :: (Double, T.State) -> Double :-* T.DState
    gradFn = undefined

    -- Mass flow rate (kg / s).
    mDot :: Double
    mDot = 290.0

    -- Specific impulse (s).
    isp :: Double
    isp = 300.0
   -}


-- | Burn the single stage rocket only.
burnSingleStage
  :: Int                          -- ^ Number of integration steps per stage.
  -> NonEmpty (Double, T.State)   -- ^ Time and state list.
burnSingleStage {- nSteps = burnStage mp0 mDry 0.0 nSteps state0 -}
  = todo (FallbackSolution Solutions.Staging.burnSingleStage)


-- | Burn the two stage rocket only.
burnTwoStage
  :: Int                         -- ^ Number of integration steps per stage.
  -> NonEmpty (Double, T.State)  -- ^ Time and state list.
burnTwoStage {- nSteps = stage1 <> stage2 -}
  = todo (FallbackSolution Solutions.Staging.burnTwoStage)


-- | Plot the comparison of velocity history for the staging options.
plotVelocityComparison :: Plot.Output -> IO ()
plotVelocityComparison output =
  let
    nSteps :: Int
    nSteps = 100

    extractVel :: (Double, T.State) -> (Double, Double)
    extractVel (t, state) = (t, T.velocity state)

    v1Stage :: [(Double, Double)]
    v1Stage = NonEmpty.toList (extractVel <$> burnSingleStage nSteps)

    v2Stage :: [(Double, Double)]
    v2Stage = NonEmpty.toList (extractVel <$> burnTwoStage nSteps)
  in
    Plot.xyChart
      output
      "Single Stage vs Two Stage Velocity Comparison"
      "Time (s)"
      "Velocity (m/s)"
      []
      [ Plot.Line "Single Stage" v1Stage
      , Plot.Line "Two Stage" v2Stage ]
