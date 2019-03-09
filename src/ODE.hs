{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}
module ODE where

import           Data.AffineSpace   (AffineSpace, Diff, (.+^))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Typeable      (Typeable)
import           Data.VectorSpace   (Scalar, VectorSpace, (*^))

import qualified Solutions.ODE
import           Todo               (FallbackSolution (..), todo)
import qualified Plot


{-

Problem 1: Euler integration.

We're going to start integrating ODEs using Euler's method, which is
the simplest possible approach. We have an equation of the form:

  dx/dt = f (t, x)

In the Rocket examples, t is almost always time, but it doesn't
always have to be. x is a collection of state variables such as mass,
position, velocity, etc.

In Euler's method, we approximate taking "a step" in t by the
following equation:

  xNext = x + dx = x + (dx/dt)*dt = x + dt * f (t, x)

Where xNext is the value of x after the step. Similarly, the value of
t itself is incremented in the same way:

  tNext = t + dt

Start below by implementing a single step of Euler integration
specialized to the Double type:

-}

-- | Single step of Euler integration, specialized to 'Double'.
--
-- Example:
--
-- >>> f (_, x) = -0.2 * x
-- >>> eulerStepDouble 1 f (2, 5)
-- (3.0,4.0)
--
-- 3.0 is the value of @t@ after the time step.
-- 4.0 is the value of @x@ after the time step.
eulerStepDouble
  :: Double                        -- ^ Step size @dt@
  -> ((Double, Double) -> Double)  -- ^ Gradient function @f (t, x)@
  -> (Double, Double)              -- ^ Time and state before the step @(t, x)@
  -> (Double, Double)              -- ^ Time and state after the step @(t, x)@
eulerStepDouble -- dt f q@(t, x)
  = todo (FallbackSolution Solutions.ODE.eulerStepDouble)

{-

Now we can extend this single step of Euler integration to a
(non-empty) sequence of time steps.

The NonEmpty.scanl function is a convenient way to accumulate
results:
  NonEmpty.scanl :: Foldable f => (b -> a -> b) -> b -> f a -> NonEmpty b

-}

-- | Integrate an ODE using Euler's method (specialized to a single 'Double'
--   state variable).
--
-- Example:
--
-- >>> f (_, x) = -0.2 * x
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
  {- Possible template:
  let
    stepFn :: (Double, Double) -> Double -> (Double, Double)
    stepFn q@(tStart, _) tEnd = -- TODO
  in
    NonEmpty.scanl stepFn (t0, x0) ts
  -}


plotEulerDoubleExpDecay :: Plot.Output -> IO ()
plotEulerDoubleExpDecay out = do
  let
    times = [0.0, 1.0 .. 10.0]
    analytic = [ (t, exp(-0.2 * t)) | t <- times ]
    f (_, x) = -0.2 * x
    numerical = NonEmpty.toList (integrateEulerDouble f 1.0 (NonEmpty.fromList times))

  Plot.plotLinesGNUPlot
    out
    (Plot.Title "Exponential Decay - Analytic vs Numerical")
    (Plot.XLabel "Time (t)")
    (Plot.YLabel "Amount (x)")
    [ (Plot.Line "Analytic Solution" analytic)
    , (Plot.Points "Numerical Solution" numerical) ]

