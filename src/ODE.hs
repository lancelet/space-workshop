{-|
Module      : ODE
Description : Integration of Ordinary Differential Equations.

Solutions for the problems in this module are contained in the
'Solutions.ODE' module.
-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}  -- re-enable after completing solutions
module ODE where

import           Control.Lens       (makeLenses, view, (^.), _1, _2)
import           Data.AdditiveGroup (AdditiveGroup)
import           Data.AffineSpace   (AffineSpace, Diff, (.+^), (.-.))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.VectorSpace   (Scalar, VectorSpace, (*^))
import           GHC.Generics       (Generic)

import qualified Plot
import qualified Solutions.ODE
import           Todo               (FallbackSolution (FallbackSolution), todo)


{-

Problem 1: Euler integration.

Euler's method is the simplest, most understandable approach to
integrating ODEs. We have an equation of the form:

  dx/dt = f (t, x)

In the Rocket examples, t is always time, while x is a collection of
state variables such as mass, position, velocity, etc. The function f
and the starting conditions (t0, x0) are provided by the user. The
job of integration is to figure out how the values of x evolve over
time. This is referred to as an "initial value problem" because the
initial values are supplied.

In Euler's method, we approximate taking "a step" in time, to evolve
the values in x, by the following equations:

  tNext = t + dt

  xNext = x + dx
        = x + (dx/dt)*dt
        = x + dt * f (t, x)

Where tNext is the time after the step and xNext is the value of x
after the step.

Start below by implementing a single step of Euler integration,
specialized to the Double type (we'll generalise soon):

-}

-- | Single step of Euler integration (specialized to 'Double').
--
-- Example:
--
-- >>> f (_, x) = -0.2 * x  -- this is an exponential decay equation
-- >>> eulerStepDouble 1 f (2, 5)
-- (3.0,4.0)
--
-- @
--   ^   ^             ^    ^  ^
--   |   |             |    |  |
--   |   |             |    |  --- x = 5 before the time step
--   |   |             |    ------ t = 2 before the time step
--   |   |             ----------- dt = 1 is the time step
--   |   ---- x = 4 after the time step
--   -------- t = 3 after the time step
-- @
--
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

-- | Integrate an ODE using Euler's method (specialized to 'Double').
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

Exponential decay has an analytic solution because it's quite simple.
But obviously the main point of numerical integration is to apply it
to problems that are complicated enough that they *don't* have a
closed-form analytical solution.

Let's examine the behaviour of Euler integration with different size
time steps:

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
    (Plot.Title "Exponential Decay - Analytic vs Euler")
    (Plot.XLabel "Time (t) - no units assigned")
    (Plot.YLabel "Amount (x) - no units assigned")
    [ (Plot.Line "Analytic Solution" (analytic (linspace 50 0.0 10.0)))
    , (Plot.Points "Euler (dt=2.0)" (numerical (linspace 6 0.0 10.0)))
    , (Plot.Points "Euler (dt=1.0)" (numerical (linspace 11 0.0 10.0)))
    , (Plot.Points "Euler (dt=0.2)" (numerical (linspace 51 0.0 10.0)))
    ]


{-

The results of this plot show that Euler's method approximates the
analytical solution. The approximation is better when smaller time
steps are taken. There is a limit to this improvement which occurs as
the cumulative errors from small floating point operations begin to
overwhelm the individual step error.

We'll see later that increasing the polynomial order of the
integration (via the Runge Kutta 4th order method - RK4) can often
improve the results more than simply taking smaller steps with the
Euler method.

Before implementing RK4, we'll take a short diversion to introduce
types from the vector-space package. These types allow us to write
algorithms like Euler and RK4 integration polymorphically.

To get a feel for these types, we will consider the problem of simple
harmonic motion. This is the idealised motion of a mass on a perfect
linear spring in the absence of all other forces.  The state space for
this system includes both position and velocity, so let's create a
custom data type for them:

-}

-- | State of a 1D simple harmonic oscillator.
data State a
  = State
    { _pos :: a  -- ^ Position.
    , _vel :: a  -- ^ Velocity.
    } deriving (Show, Eq)
makeLenses ''State

{-

In Euler's method, we can see that we're using a difference in the
state, which is either a delta or gradient. We can represent both with
a new type.  We can derive some type class instances for free:

-}

-- | Difference or gradient in the state of a 1D simple harmonic
--   oscillator.
data DState a
  = DState
    { _dpos :: a  -- ^ Delta or gradient in position.
    , _dvel :: a  -- ^ Delta or gradient in velocity.
    } deriving (Show, Eq, Generic, AdditiveGroup, VectorSpace)
makeLenses ''DState

{- $

Now the vector-space package comes in to play to relate these two
types.  The state space is represented as an affine space, while the
gradient of the state is its associated vector space.

If we subtract two State values, we get a DState value. We can
multiply DStates by scalars and can add them back on to State values.

Example:

>>> State 1.0 2.0 .+^ 2.0 *^ DState 3.0 4.0
State {_pos = 7.0, _vel = 10.0}

This is behaving as expected: (1 + 2*3 = 7, 2 + 2*4 = 10)

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

(Using AffineSpace and VectorSpace to represent the problem does have
some limitations. In particular, introducing units is difficult
because DState is used to represent both a delta in state (*after*
being multiplied by a time increment), and the gradient of
state. These have different units!  But the approach is still safer
than what appears in most languages.)

So, let's generalize Euler's method to work on an AffineSpace. The
appropriate signature is given below:

-}

-- | Single step of Euler integration.
--
-- Example:
--
-- >>> f (_, x) = DState (x^.vel) (-0.2*x^.pos)  -- SHM
-- >>> eulerStep 1 f (0.0, State 1 0)
-- (1.0,State {_pos = 1.0, _vel = -0.2})
eulerStep
  :: ( -- as is an AffineSpace. This is the system state.
       AffineSpace as
       -- vs is the associated vector space (Diff) of as, and it's
       -- also a vector space. This is the delta/gradient of the
       -- system state.
     , vs ~ Diff as, VectorSpace vs
       -- s is the scalar that can multiply elements of vs, and
       -- it is a Num.
     , s ~ Scalar vs, Num s )
  => s                -- ^ Step size @dt@
  -> ((s, as) -> vs)  -- ^ Gradient function @f (x, t)@
  -> (s, as)          -- ^ Time and state before the step @(t, x)@
  -> (s, as)          -- ^ Time and state after the step @(t, x)@
eulerStep -- dt f q@(t, x)
  = todo (FallbackSolution Solutions.ODE.eulerStep)

{-

All steppers for integration that we'll use will have this same
signature, so it's convenient to create a type alias for it:

-}

-- | Stepper function used in ODE integration.
type Stepper s as vs
   = s                -- ^ Step size @dt@.
  -> ((s, as) -> vs)  -- ^ Gradient function @f (x, t)@.
  -> (s, as)          -- ^ Time and state before the step @(t, x)@
  -> (s, as)          -- ^ Time and state after the step @(t, x)@

{-

Now we can also generalize the integration driver:

-}

-- | Integrate an ODE.
--
-- Example:
--
-- >>> x0 = State 1.0 0.0
-- >>> ts = NonEmpty.fromList [ 0.0, 1.0, 2.0, 3.0 ]
-- >>> f (_, x) = DState (x^.vel) (-0.2*x^.pos)  -- SHM
-- >>> view (_2 . pos) <$> integrate eulerStep x0 ts f
-- 1.0 :| [1.0,0.8,0.4]
integrate
  :: forall as vs s.
     ( AffineSpace as
     , vs ~ Diff as, VectorSpace vs
     , s ~ Scalar vs, Fractional s )
  => Stepper s as vs    -- ^ Stepper function
  -> as                 -- ^ Initial state @x0@
  -> NonEmpty s         -- ^ NonEmpty of @t@ values
  -> ((s, as) -> vs)    -- ^ Gradient function @f (t, x)@
  -> NonEmpty (s, as)   -- ^ NonEmpty of @(t, x)@ values
integrate -- stepper x0 ts f
  = todo (FallbackSolution Solutions.ODE.integrate)

{-

Let's see how Euler integration does on simple harmonic motion by
plotting some results:

-}

plotEulerSHM :: Plot.Output -> IO ()
plotEulerSHM out = do
  let
    analytic :: [Double] -> [(Double, Double)]
    analytic times = [ (t, cos (t * (sqrt 0.2))) | t <- times ]

    numerical :: [Double] -> [(Double, Double)]
    numerical times =
      let
        tne = NonEmpty.fromList times
        f (_, state) = DState
                       { _dpos = state^.vel
                       , _dvel = -0.2*state^.pos }
        state0 = State { _pos = 1.0, _vel = 0.0 }
        timeAndPos r = (r^._1, r^._2.pos)
      in
        NonEmpty.toList $ timeAndPos <$> integrate eulerStep state0 tne f

  Plot.plotXYChart out $ Plot.XYChart
    (Plot.Title "Simple Harmonic Motion - Analytic vs Euler")
    (Plot.XLabel "Time (t) - no units assigned")
    (Plot.YLabel "Position (pos) - no units assigned")
    [ (Plot.Line "Analytic Solution" (analytic (linspace 200 0.0 20.0)))
    , (Plot.Points "Euler (dt=1.0)" (numerical (linspace 21 0.0 20.0)))
    , (Plot.Points "Euler (dt=0.5)" (numerical (linspace 41 0.0 20.0)))
    , (Plot.Points "Euler (dt=0.1)" (numerical (linspace 201 0.0 20.0)))
    ]

{-

We can see that, again, Euler integration approximates the analytical
solution, with a smaller step size improving the match.

-}

{-

Problem 2: 4th-order Runge-Kutta Integration (RK4)

As suggested above, it's possible to do better than Euler integration
for smooth problems by increasing the polynomial order of the
integration. In essence, this uses evaluations of the gradient
function between the two ends of the time step to better fit the
function that is being integrated and better approximate the value
after the step.

The equations for a single step of RK4 are as follows:

  k1 = dt * f (t, x)
  k2 = dt * f (t + 0.5*dt, x + 0.5*k1)
  k3 = dt * f (t + 0.5*dt, x + 0.5*k2)
  k4 = dt * f (t + dt, x + k3)

  xNext = (1/6)*k1 + (1/3)*k2 + (1/3)*k3 + (1/6)*k4
  tNext = t + dt

Implement these in a stepper function for RK4:

-}

-- | Single step of 4th-order Runge-Kutta integration.
rk4Step
  :: ( AffineSpace as
     , vs ~ Diff as, VectorSpace vs
     , s ~ Scalar vs, Fractional s )
  => s                -- ^ Step size @dt@
  -> ((s, as) -> vs)  -- ^ Gradient function @f (x, t)@
  -> (s, as)          -- ^ Time and state before the step @(t, x)@
  -> (s, as)          -- ^ Time and state after the step @(t, x)@
rk4Step -- dt f (t, x)
  = todo (FallbackSolution Solutions.ODE.rk4Step)

{-

Finally, let's see how RK4 compares with Euler for the SHM example.
To make the comparison more fair, we'll plot RK4 against a Euler
scheme that uses four times as many steps, since the RK4 scheme is
doing four function evaluations for each step it takes:

-}

plotSHMComparison :: Plot.Output -> IO ()
plotSHMComparison out = do
  let
    analytic :: [Double] -> [(Double, Double)]
    analytic times = [ (t, cos (t * (sqrt 0.2))) | t <- times ]

    numeric
      :: Stepper Double (State Double) (DState Double)
      -> [Double]
      -> [(Double, Double)]
    numeric stepper times =
      let
        tne = NonEmpty.fromList times
        timeAndPos r = (r^._1, r^._2.pos)
        f (_, state) = DState
                       { _dpos = state^.vel
                       , _dvel = -0.2*state^.pos }
        state0 = State { _pos = 1.0, _vel = 0.0 }
      in
        NonEmpty.toList $ timeAndPos <$> integrate stepper state0 tne f

    euler :: [Double] -> [(Double, Double)]
    euler = numeric eulerStep

    rk4 :: [Double] ->  [(Double, Double)]
    rk4 = numeric rk4Step

  Plot.plotXYChart out $ Plot.XYChart
    (Plot.Title "Simple Harmonic Motion - Analytic vs Numerical")
    (Plot.XLabel "Time (t) - no units assigned")
    (Plot.YLabel "Position (pos) - no units assigned")
    [ (Plot.Line "Analytic Solution" (analytic (linspace 200 0.0 20.0)))
    , (Plot.Points "Euler (dt=0.5)" (euler (linspace 41 0.0 20.0)))
    , (Plot.Points "RK4 (dt=2.0)" (rk4 (linspace 11 0.0 20.0)))
    ]

{-

This plot shows that, for approximately the same amount of work, the
RK4 integration is closer to the analytical solution than Euler
integration. This is often the case for smooth functions. There are
higher-order Runge-Kutta integrators that might be used in practice,
and other alternatives too, but RK4 is satisfactory for the rest of
the workshop.

In practice, it is also often beneficial to use an RK scheme with an
adaptive step size, so that the integrator is able to take large steps
when the function is very smooth and small steps when the gradient is
changing rapidly. However, for the sake of simplicity, we will not
implement adaptive step size in this workshop.


Some existing Hackage implementations of ODE solvers include:

* The numeric-ode package. This package does not make a distinction
  between AffineSpace and VectorSpace as we have done above, and
  instead treats both as the same data type (which is similar to the
  treatment found in SciPy, Matlab, Jula, etc.):
  http://hackage.haskell.org/package/numeric-ode

* HMatrix bindings to the GNU Scientific Library:
  http://hackage.haskell.org/package/hmatrix-gsl-0.19.0.1/docs/Numeric-GSL-ODE.html

* Raw bindings to the GNU Scientific Library:
  http://hackage.haskell.org/package/bindings-gsl-0.2.1/docs/Bindings-Gsl-OrdinaryDifferentialEquations.html

* An older Runge-Kutta package. This specializes both the AffineSpace
  and VectorSpace to [Double].
  http://hackage.haskell.org/package/rungekutta

Unfortunately, it's not really possible at this stage to point a
newcomer to a comprehensive package for solving ODEs that integrates
well with packages like vector-space, linear, vector and so on.

There are also Euler integrators in several FRP libraries, but at the
time of writing, I am not aware of any FRP libraries that provide more
accurate integrators such as RK4.

-}

{- $setup
This is setup for doctests.

>>> :set -XFlexibleContexts
-}
