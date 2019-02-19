{-# LANGUAGE ScopedTypeVariables #-}

module ODE.Examples where

import           Control.Lens                              ((.=))
import qualified Data.List.NonEmpty                        as NonEmpty
import qualified Graphics.Rendering.Chart.Backend.Diagrams as ChartDiagrams
import qualified Graphics.Rendering.Chart.Easy             as Chart

import qualified ODE.FixedStepV                            as FixedStep

-------------------------------------------------------------------------------
-- Simple Harmonic Oscillator
-------------------------------------------------------------------------------


data SHMParameters
  = SHMParameters
    { mass    :: Double
    , kSpring :: Double
    }


shm :: SHMParameters -> (Double, (Double, Double)) -> (Double, Double)
shm params (_, (x, v)) = (xdot, vdot)
  where
    xdot = v
    vdot = -k * x / m

    m = mass params
    k = kSpring params


shmAnalytic :: SHMParameters -> Double -> (Double, Double)
shmAnalytic params t = (t, cos (omega * t))
  where
    omega = sqrt (k / m)

    m = mass params
    k = kSpring params


plotSHM :: IO ()
plotSHM = do

  let

    y0 = (1.0, 0.0)
    times = [0.0, 0.1 .. 15.0]
    timesAnalytic = [0.0, 0.01 .. 15.0]
    params = SHMParameters { mass = 1.0, kSpring = 3.0 }

    analytic = fmap (shmAnalytic params) timesAnalytic

    euler :: [(Double, Double)]
    euler = fmap (\(t, (x, _)) -> (t, x))
      $ NonEmpty.toList
      $ FixedStep.integrate FixedStep.eulerStep y0 (NonEmpty.fromList times) (shm params)

    rk4 :: [(Double, Double)]
    rk4 = fmap (\(t, (x, _)) -> (t, x))
      $ NonEmpty.toList
      $ FixedStep.integrate FixedStep.rk4Step y0 (NonEmpty.fromList times) (shm params)

  ChartDiagrams.toFile Chart.def "ode_examples_shm.svg" $ do
    Chart.layout_title .= "Simple Harmonic Oscillator - Analytic vs Numerical"
    Chart.layout_x_axis . Chart.laxis_title .= "Time (s)"
    Chart.layout_y_axis . Chart.laxis_generate .= Chart.scaledAxis Chart.def (-4, 4)
    Chart.layout_y_axis . Chart.laxis_title .= "Displacement (m)"
    Chart.plot (Chart.line "Analytic" [analytic])
    Chart.plot (Chart.points "Euler" euler)
    Chart.plot (Chart.points "RK4" rk4)
