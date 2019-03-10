{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module ODE where

import           Control.Lens                      (makeLenses, (^.))
import           Data.AdditiveGroup                (AdditiveGroup)
import           Data.AffineSpace                  (AffineSpace, Diff, (.+^),
                                                    (.-.))
import           Data.List.NonEmpty                (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                as NonEmpty
import           Data.Typeable                     (Typeable)
import           Data.VectorSpace                  (Scalar, VectorSpace, (*^))
import           GHC.Generics                      (Generic)
import           Numeric.Units.Dimensional.Prelude (Acceleration, Length,
                                                    Velocity, second, (*~))
import qualified Numeric.Units.Dimensional.Prelude as Dim

import           Orphans
import qualified Plot
import qualified Solutions.ODE
import           Todo                              (FallbackSolution (..), todo)


{-

Problem 1: Euler integration.

We're going to start integrating ODEs using Euler's method, which is
the simplest possible approach. We have an equation of the form:

  dx/dt = f (t, x)

In the Rocket examples, t is always time, while x is a collection of
state variables such as mass, position, velocity, etc. The function f
and the starting conditions (t0, x0) are provided by the user. The
job of integration is to figure out how the values of x evolve over
time. This is referred to as an "initial value problem" because the
initial values are supplied.

In Euler's method, we approximate taking "a step" in time, to evolve
the values in x, by the following equation:

  xNext = x + dx
        = x + (dx/dt)*dt
        = x + dt * f (t, x)

Where xNext is the value of x after the step. Similarly, the value of
time itself is incremented in the same way, but in the case of time
we of course know the increment exactly:

  tNext = t + dt

Start below by implementing a single step of Euler integration
specialized to the Double type (we'll generalise soon):

-}

-- | Single step of Euler integration, specialized to 'Double'.
--
-- Example:
--
-- >>> f (_, x) = -0.2 * x  -- this is an exponential decay equation
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

Extend the single step of Euler integration to a (non-empty) sequence of
time steps.

The NonEmpty.scanl function is a convenient way to drive the computation
and accumulate results:
  NonEmpty.scanl :: Foldable f => (b -> a -> b) -> b -> f a -> NonEmpty b

-}

-- | Integrate an ODE using Euler's method (specialized to a single 'Double'
--   state variable).
--
-- Example:
--
-- >>> f (_, x) = -0.2 * x  -- this is an exponential decay equation
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
  {- Template:
  let
    stepFn :: (Double, Double) -> Double -> (Double, Double)
    stepFn q@(tStart, _) tEnd = -- TODO
  in
    NonEmpty.scanl stepFn (t0, x0) ts
  -}

{-

Now Euler integration is implemented, let's see how it behaves. We're
going to plot it against an analytic solution for exponential decay.

(Exponential decay has an analytic solution because it's so simple.
But obviously the main point of numerical integration is to apply it
to problems that are complicated enough that they *don't* have a
closed-form analytical solution.)

Let's examine the behaviour of Euler integration with different size
time steps.

-}


-- | Linearly-spaced samples.
--
-- Example:
--
-- >>> linspace 6 0.0 10.0
-- [0.0,2.0,4.0,6.0,8.0,10.0]
linspace
  :: Fractional a
  => Int   -- ^ Number of samples.
  -> a     -- ^ Start of the sample range (== first sample).
  -> a     -- ^ End of the sample range (== last sample for n > 2).
  -> [a]   -- ^ Samples.
linspace n xStart xEnd =
  let
    m = n - 1
    m' = fromIntegral m
    range = xEnd - xStart
    f i = fromIntegral i * range / m' + xStart
  in
    [ f i | i <- [0 .. m] ]



-- | Plots an exponential decay ODE; comparing an analytical result against
--   Euler integration.
plotEulerDoubleExpDecay :: Plot.Output -> IO ()
plotEulerDoubleExpDecay out = do
  let
    -- the analytic equation; takes a list of times; produces a list of
    --  (time, x)
    analytic :: [Double] -> [(Double, Double)]
    analytic times = [ (t, exp(-0.2 * t)) | t <- times ]

    -- the numerical solution; takes a list of times; produces a list of
    --  (time, x)
    numerical :: [Double] -> [(Double, Double)]
    numerical times =
      let
        f (_, x) = -0.2 * x   -- the gradient function
      in
        NonEmpty.toList $
        integrateEulerDouble f 1.0 (NonEmpty.fromList times)

  -- plot everything
  Plot.plotXYChart out $ Plot.XYChart
    (Plot.Title "Exponential Decay - Analytic vs Numerical")
    (Plot.XLabel "Time (t) - no units assigned")
    (Plot.YLabel "Amount (x) - no units assigned")
    [ (Plot.Line "Analytic Solution" (analytic (linspace 50 0.0 10.0)))
    , (Plot.Points "Numerical (dt=2.0)" (numerical (linspace 6 0.0 10.0)))
    , (Plot.Points "Numerical (dt=1.0)" (numerical (linspace 11 0.0 10.0)))
    , (Plot.Points "Numerical (dt=0.2)" (numerical (linspace 51 0.0 10.0)))
    ]


{-

The results of this plot show that Euler's method does indeed provide
an approximation of the analytical solution. The approximation becomes
better as smaller time steps are taken, although there is a limit to
this improvement which occurs as the cumulative errors from small
floating point operations can overwhelm the individual step error.

We'll see later that increasing the polynomial order of the
integration (via the Runge Kutta 4th order method - RK4) can often
improve the results more than simply taking smaller steps with the
Euler method.

Before implementing RK4, we'll take a short diversion to introduce
types from the vector-spaces package. These types allow us to write
algorithms like Euler and RK4 integration quite generically.

To get a feel for these types, we're going to look at an example
problem of simple harmonic motion. This is the idealised motion of a
mass on a perfect linear spring in the absence of all other forces.
The state space for this system includes both position and velocity,
so let's create a custom data type for them:

-}

-- | State of a 1D simple harmonic oscillator.
data State a
  = State
    { _pos :: a  -- ^ Position.
    , _vel :: a  -- ^ Velocity.
    }
makeLenses ''State

{-

In Euler's method, we can see that we're using a difference in the
state: a delta or gradient. We can represent the delta with a new type.
We get some type class instances for free:

-}

-- | Difference or gradient in the state of a 1D simple harmonic
--   oscillator.
data DState a
  = DState
    { _dpos :: a  -- ^ Delta or gradient in position.
    , _dvel :: a  -- ^ Delta or gradient in velocity.
    } deriving (Generic, AdditiveGroup, VectorSpace)
makeLenses ''DState

{-

Now the vector-space package comes in to play to relate these two
types.  The state space is represented as an affine space, while the
gradient of the state is its associated vector space.

If we subtract two State values, we get a DState value. We can
multiply DStates by scalars and can add them back on to State values:

-}

instance (AdditiveGroup a, Num a) => AffineSpace (State a) where
  -- associated vector space
  type Diff (State a) = DState a
  -- subtraction of states.
  s1 .-. s2 = DState
              { _dpos = s1^.pos - s2^.pos
              , _dvel = s1^.vel - s2^.vel }
  -- Addition of a State and a DState
  s .+^ ds = State
             { _pos = s^.pos + ds^.dpos
             , _vel = s^.vel + ds^.dvel }

{-

(It may be important to mention at this point that the analogy here is
a bit limited in corner cases. It's "traditional" in engineering
applications to treat DState as a vector space. Engineers often treat
State as a vector space too, and don't even distinguish it from
DState! But there are problems if you're looking for complete
consistency, which is something we should care about!

For example, we will be using DState here to represent both a delta in
state *and* a gradient, which have different units. Thus if we want to
introduce units, we'll encounter consistency problems (try it!).

There are also questions about whether some operations have any
physical meaning. For example, what does the length of a DState vector
signify? Or a dot product between a pair of them?

These are interesting questions, which really ought to be investigated
by more mathematically-minded Haskellers. However, in the short term,
these representations are more than adequate and provide additional
type safety over approaches used in other languages.)

So, let's generalize Euler's method to work on an AffineSpace. The
appropriate signature is given below:

-}

