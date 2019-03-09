{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Solutions.ODE where

import           Data.AffineSpace   (AffineSpace, Diff, (.+^))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.VectorSpace   (Scalar, VectorSpace, (*^))


-- | Single steo of Euler integration, specialized to 'Double'.
eulerStepDouble
  :: Double                        -- ^ Step size
  -> ((Double, Double) -> Double)  -- ^ Gradient function @f@
  -> (Double, Double)              -- ^ State before the step @(x, y)@
  -> (Double, Double)              -- ^ State after the step @(x, y)@
eulerStepDouble dt f q@(t, x) = (t + dt, x + (dt*f q))


-- | Integrate an ODE using Euler's method (specialized to a single 'Double'
--   state variable).
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
