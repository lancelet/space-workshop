{-|
Module      : Staging
Description : Simulating staging of rockets.
-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- re-enable after completing solutions
module Staging where

import           Data.LinearMap     ((:-*), linear)
import           Data.List.NonEmpty (NonEmpty)
import           Data.VectorSpace   ((^*))

import qualified ODE
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


burnStage
  :: Double                      -- ^ Initial propellant mass (kg).
  -> Double                      -- ^ Dry mass of the current stage (kg).
  -> Double                      -- ^ Total mass of remaining mass of stages (kg).
  -> Int                         -- ^ Number of integration steps.
  -> T.State                     -- ^ Initial state.
  -> NonEmpty (Double, T.State)  -- ^ List of time and state tuples.
burnStage = undefined
  

burnSingleStage :: Int -> NonEmpty T.State
burnSingleStage = undefined


burnTwoStage :: Int -> NonEmpty T.State
burnTwoStage = undefined
