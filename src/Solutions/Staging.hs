{-|
Module      : Solutions.Staging
Description : Solutions for the 'Staging' module.
-}
{-# LANGUAGE TypeOperators #-}
module Solutions.Staging where

import           Data.LinearMap     ((:-*), linear)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.VectorSpace   ((^*))

import qualified ODE
import qualified Staging.Types      as T


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
equationOfMotion mDot isp mDry mRemaining (_, state) = linear ((^*) grad)
  where
    -- grad is a DState for a unit time increment
    grad :: T.DState
    grad = T.DState
           { T.dPropellantMass = -mDot
           , T.dPosition       = v
           , T.dVelocity       = g0 * isp * mDot / (mp + mDry + mRemaining)
           }

    v  = T.velocity state
    mp = T.propellantMass state

    g0 :: Double  -- m/s^2
    g0 = 9.80665


-- | Burn a stage of a rocket (can be the first or second stage).
burnStage
  :: Double                      -- ^ Initial propellant mass (kg).
  -> Double                      -- ^ Dry mass of the current stage (kg).
  -> Double                      -- ^ Total mass of remaining mass of stages (kg).
  -> Int                         -- ^ Number of integration steps.
  -> T.State                     -- ^ Initial state.
  -> NonEmpty (Double, T.State)  -- ^ List of time and state tuples.
burnStage mp0 mDry mRemaining nSteps state0
  = ODE.integrate ODE.rk4Step state0 times gradFn
  where
    -- Time at the end of the burn (s).
    endBurnTime :: Double
    endBurnTime = mp0 / mDot
    
    -- Evaluation times (s).
    times :: NonEmpty Double
    times = NonEmpty.fromList (ODE.linspace nSteps 0 endBurnTime)

    -- Gradient function for ODE solver.
    gradFn :: (Double, T.State) -> Double :-* T.DState
    gradFn = equationOfMotion mDot isp mDry mRemaining

    -- Mass flow rate (kg / s).
    mDot :: Double
    mDot = 290.0

    -- Specific impulse (s).
    isp :: Double
    isp = 300.0
  

-- | Burn the single stage rocket only.
burnSingleStage
  :: Int                          -- ^ Number of integration steps per stage.
  -> NonEmpty (Double, T.State)   -- ^ Time and state list.
burnSingleStage nSteps = burnStage mp0 mDry 0.0 nSteps state0
  where
    -- Initial state.
    state0 :: T.State
    state0 = T.State
             { T.propellantMass = mp0
             , T.position       = 0.0
             , T.velocity       = 0.0
             }
    
    -- Starting propellant mass (kg).
    mp0 :: Double
    mp0 = 500000.0

    -- Dry mass of the stage (kg).
    mDry :: Double
    mDry = 55556.0


-- | Burn the two stage rocket only.
burnTwoStage
  :: Int                         -- ^ Number of integration steps per stage.
  -> NonEmpty (Double, T.State)  -- ^ Time and state list.
burnTwoStage nSteps = stage1 <> stage2
  where
    -- Results from burning the first stage.
    stage1 :: NonEmpty (Double, T.State)
    stage1 = burnStage mp0 mDry (mp0 + mDry) nSteps state01

    -- Results from burning the second stage.
    stage2 :: NonEmpty (Double, T.State)
    stage2 = burnStage mp0 mDry 0 nSteps state02

    -- Initial state of the first stage.
    state01 :: T.State
    state01 = T.State
              { T.propellantMass = mp0
              , T.position       = 0.0
              , T.velocity       = 0.0
              }

    -- Initial state of the second stage.
    state02 :: T.State
    state02 = (snd (NonEmpty.last stage1)) { T.propellantMass = mp0 }

    -- Starting propellant mass for each stage.
    mp0 :: Double
    mp0 = 250000.0

    -- Dry mass for each stage.
    mDry :: Double
    mDry = 27778.0
