{-|
Module      : Examples.ODEExamples
Description : Plots of ODE integration.

The examples in this module accompany the problems outlined in the
'ODE' module.
-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Examples.ODEExamples where

import           Control.Lens          (makeLenses, (^.))
import           Data.AdditiveGroup    (AdditiveGroup)
import           Data.AffineSpace      (AffineSpace, Diff, (.+^), (.-.))
import           Data.LinearMap        ((:-*), linear)
import           Data.List.NonEmpty    (NonEmpty)
import qualified Data.List.NonEmpty    as NonEmpty
import           Data.Metrology.Vector (qNegate, qSqrt, (%), (|*|), (|+|),
                                        (|-|), (|/|))
import           Data.Units.SI.Parser  (si)
import           Data.VectorSpace      (VectorSpace, (*^))
import           GHC.Generics          (Generic)
import           Orphans               ()

import qualified ODE
import qualified Plot
import qualified Units                 as U


-------------------------------------------------------------------------------
-- Exponential Decay Example
-------------------------------------------------------------------------------


-- | Plot Euler integration of an exponential decay ODE.
--
-- The example here is decay of Plutonium-238, with a half-life of 87.7 years.
plotEulerDoubleExpDecay :: Plot.Output -> IO ()
plotEulerDoubleExpDecay out = do
  let
    tHalf = 87.7            -- half-life of Pu-238 (years)
    ln = logBase (exp 1)
    lambda = ln 2 / tHalf   -- decay constant of Pu-238 (1/years)

    -- the analytic equation; takes a list of times; produces a list of
    --  (time, x)
    analytic :: [Double] -> [(Double, Double)]
    analytic times = [ (t, exp(-lambda * t)) | t <- times ]

    -- the numerical solution; takes a list of times; produces a list of
    --  (time, x)
    numerical :: [Double] -> [(Double, Double)]
    numerical times =
      let
        f (_, x) = -lambda * x   -- the gradient function
      in
        NonEmpty.toList $
        ODE.integrateEulerDouble f 1.0 (NonEmpty.fromList times)

  -- plot everything
  Plot.xyChart
    out
    "Radioactive Decay of Pu-238 - Analytical vs Euler"
    "Time (t) - years"
    "Amount (N) - fraction of original"
    []
    [ Plot.Line "Analytical Solution" (analytic (ODE.linspace 50 0.0 200.0))
    , Plot.Points "Euler (dt=40 years)" (numerical (ODE.linspace 6 0.0 200.0))
    , Plot.Points "Euler (dt=20 years)" (numerical (ODE.linspace 11 0.0 200.0))
    , Plot.Points "Euler (dt=4 years)" (numerical (ODE.linspace 51 0.0 200.0)) ]


-------------------------------------------------------------------------------
-- Simple Harmonic Oscillator Example
-------------------------------------------------------------------------------


-- | State of a 1D simple harmonic oscillator.
--
-- The state uses lenses and has SI units for both position and
-- velocity. @a@ is the underlying numeric type.
data StateSHM a
  = StateSHM
    { _pos :: U.Length a    -- ^ Position.
    , _vel :: U.Velocity a  -- ^ Velocity.
    } deriving (Show, Eq)
makeLenses ''StateSHM


-- | Delta in the state of a 1D simple harmonic oscillator.
--
-- The delta uses lenses and has SI units for both the difference in
-- position and difference in velocity. @a@ is the underlying numeric
-- type.
data DStateSHM a
  = DStateSHM
    { _dpos :: U.Length a    -- ^ Delta in position.
    , _dvel :: U.Velocity a  -- ^ Delta in velocity.
    } deriving (Show, Eq, Generic, AdditiveGroup, VectorSpace)
makeLenses ''DStateSHM


-- Connects State and DState into an AffineSpace/VectorSpace structure.
instance (AdditiveGroup a) => AffineSpace (StateSHM a) where
  type Diff (StateSHM a) = DStateSHM a
  s1 .-. s2 = DStateSHM
              { _dpos = s1^.pos |-| s2^.pos
              , _dvel = s1^.vel |-| s2^.vel
              }
  s .+^ ds = StateSHM
             { _pos = s^.pos |+| ds^.dpos
             , _vel = s^.vel |+| ds^.dvel
             }


-- | Plot Euler integration of the Simple Harmonic Motion example.
--
-- This example uses units and involves free conversion between them.
plotEulerSHM :: Plot.Output -> IO ()
plotEulerSHM out = do
  let
    -- parameters
    ti =    0 % [si| ms |]     -- initial simulation time
    tf = 1500 % [si| ms |]     -- final simulation time
    x0 =   10 % [si| mm |]     -- initial position
    v0 =    0 % [si| m/s |]    -- initial velocity
    k  =   10 % [si| mN/mm |]  -- spring stiffness
    m  =  500 % [si| g |]      -- mass
    omega = qSqrt (k |/| m)    -- angular frequency

    -- ODE we're solving
    shmODE :: (U.Time Double, StateSHM Double) -> (U.Time Double :-* DStateSHM Double)
    shmODE (_, state) = linear $ \dt ->
      DStateSHM { _dpos = state^.vel |*| dt
                , _dvel = qNegate(state^.pos |*| k |/| m) |*| dt }

    -- analytical solution
    analytic :: [U.Time Double] -> [(U.Time Double, U.Length Double)]
    analytic times = [ (t, x0 |*| cos (t |*| omega)) | t <- times ]

    -- numerical solution
    numerical :: [U.Time Double] -> [(U.Time Double, U.Length Double)]
    numerical times =
      let
        state0 = StateSHM { _pos = x0, _vel = v0 }
        tstates = ODE.integrate ODE.eulerStep
                                state0
                                (NonEmpty.fromList times)
                                shmODE
      in
        NonEmpty.toList $ (\(t, s) -> (t, s^.pos)) <$> tstates

  Plot.xyChartUnits
    out
    "Simple Harmonic Motion - Analytical vs Euler"
    "Time (ms)"
    "Position (mm)"
    ( [si| ms |], [si| mm |] )
    [ Plot.Line "Analytical Solution" $ analytic (ODE.linspace 200 ti tf)
    , Plot.Points "Euler (dt=75.0 ms)" $ numerical (ODE.linspace 20 ti tf)
    , Plot.Points "Euler (dt=37.5 ms)" $ numerical (ODE.linspace 40 ti tf)
    , Plot.Points "Euler (dt=7.5 ms)" $ numerical (ODE.linspace 200 ti tf) ]


-- | Plot comparison of Euler and RK4 integration of the Simple
-- Harmonic Motion example.
plotSHMComparison :: Plot.Output -> IO ()
plotSHMComparison out = do
  let
    -- parameters
    ti =    0 % [si| s |]      -- initial simulation time
    tf = 1500 % [si| ms |]     -- final simulation time
    x0 =   10 % [si| mm |]     -- initial position
    v0 =    0 % [si| m/s |]    -- initial velocity
    k  =   10 % [si| mN/mm |]  -- spring stiffness
    m  =  500 % [si| g |]      -- mass
    omega = qSqrt (k |/| m)    -- angular frequency

    -- ODE we're solving
    shmODE :: (U.Time Double, StateSHM Double) -> (U.Time Double :-* DStateSHM Double)
    shmODE (_, state) = linear $ \dt ->
      DStateSHM { _dpos = state^.vel |*| dt
                , _dvel = qNegate(state^.pos |*| k |/| m) |*| dt }

    -- analytical solution
    analytic :: [U.Time Double] -> [(U.Time Double, U.Length Double)]
    analytic times = [ (t, x0 |*| cos (t |*| omega)) | t <- times ]

    -- numerical solution
    numerical
      :: ODE.Stepper (U.Time Double) (StateSHM Double)
      -> [U.Time Double]
      -> [(U.Time Double, U.Length Double)]
    numerical stepper times =
      let
        state0 = StateSHM { _pos = x0, _vel = v0 }
        tstates = ODE.integrate stepper
                                state0
                                (NonEmpty.fromList times)
                                shmODE
      in
        NonEmpty.toList $ (\(t, s) -> (t, s^.pos)) <$> tstates

    -- Euler and RK4 numerical solutions
    euler = numerical ODE.eulerStep
    rk4   = numerical ODE.rk4Step

  Plot.xyChartUnits
    out
    "Simple Harmonic Motion - Analytical, Euler and RK4"
    "Time (ms)"
    "Position (mm)"
    ( [si| ms |], [si| mm |] )
    [ Plot.Line "Analytical Solution" $ analytic (ODE.linspace 200 ti tf)
    , Plot.Points "Euler (dt=37.5 ms)" $ euler (ODE.linspace 40 ti tf)
    , Plot.Points "RK4 (dt=150.0 ms)" $ rk4 (ODE.linspace 10 ti tf) ]


-------------------------------------------------------------------------------
-- Vertical throw example
-------------------------------------------------------------------------------


type StateVT = (Double, Double)   -- ^ (height, velocity)
type DStateVT = (Double, Double)  -- ^ (dheight, dvelocity)


-- Plots height vs time for a vertial throw; terminating at apogee.
--
-- The tEpsilon and dt values are chosen so that the time step is bisected
-- several times to find the apogee with an accuracy of 0.001s.
plotVerticalThrow :: Plot.Output -> IO ()
plotVerticalThrow out = do

  let
    -- ODE to solve; non-terminating
    vtODE :: (Double, StateVT) -> Double :-* DStateVT
    vtODE (_, (_, v)) = linear $ \dt -> dt *^ (v, -9.81)

    -- Compose a termination condition with the ODE
    vtODEt :: (Double, StateVT) -> Maybe (Double :-* DStateVT)
    vtODEt ts@(_, (_, v)) = if v > 0 then Just (vtODE ts) else Nothing

    -- Required time accuracy for termination
    tEpsilon :: Double
    tEpsilon = 0.001 -- seconds

    -- Initial state
    state0 :: StateVT
    state0 = (0, 29)

    -- Run the ODE integration
    states :: Double -> NonEmpty (Double, StateVT)
    states dt = ODE.integrateTerminating ODE.rk4StepTerminating
                                         tEpsilon dt
                                         (0, state0)
                                         vtODEt

    -- List of (time, height)
    heights :: Double -> [(Double, Double)]
    heights dt = (\(t, (h, _)) -> (t, h)) <$> (NonEmpty.toList (states dt))

  Plot.xyChart
    out
    "Vertical Throw Example - Bisection of Termination"
    "Time (s)"
    "Height (m)"
    []
    [ Plot.Line "RK4 (dt = 0.01s)" (heights 0.01)
    , Plot.Points "RK4 (dt = 0.5s)" (heights 0.5) ]
