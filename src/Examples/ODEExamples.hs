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

import           Control.Lens          (makeLenses, (<&>), (^.), _1, _2)
import           Data.AdditiveGroup    (AdditiveGroup)
import           Data.AffineSpace      (AffineSpace, Diff, (.+^), (.-.))
import           Data.LinearMap        ((:-*), linear)
import qualified Data.List.NonEmpty    as NonEmpty
import           Data.Metrology.Vector (qNegate, qSqrt, ( # ), (%), (*|), (|*|),
                                        (|+|), (|-|), (|/|))
import           Data.Units.SI.Parser  (si)
import           Data.VectorSpace      (VectorSpace)
import           GHC.Generics          (Generic)
import Orphans ()

import qualified ODE
import qualified Plot
import qualified Units                 as U

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
        ODE.integrateEulerDouble f 1.0 (NonEmpty.fromList times)

  -- plot everything
  Plot.plotXYChart
    out
    "Exponential Decay - Analytic vs Euler"
    "Time (t) - no units assigned"
    "Amount (x) - no units assigned"
    [ (Plot.Line "Analytic Solution" (analytic (ODE.linspace 50 0.0 10.0)))
    , (Plot.Points "Euler (dt=2.0)" (numerical (ODE.linspace 6 0.0 10.0)))
    , (Plot.Points "Euler (dt=1.0)" (numerical (ODE.linspace 11 0.0 10.0)))
    , (Plot.Points "Euler (dt=0.2)" (numerical (ODE.linspace 51 0.0 10.0)))
    ]


-- | State of a 1D simple harmonic oscillator.
--
-- The state uses lenses and has SI units for both position and
-- velocity.
data State a
  = State
    { _pos :: U.Length a    -- ^ Position.
    , _vel :: U.Velocity a  -- ^ Velocity.
    } deriving (Show, Eq)
makeLenses ''State


-- | Delta in the state of a 1D simple harmonic oscillator.
--
-- The delta uses lenses and has SI units for both the difference in
-- position and difference in velocity.
data DState a
  = DState
    { _dpos :: U.Length a    -- ^ Delta in position.
    , _dvel :: U.Velocity a  -- ^ Delta in velocity.
    } deriving (Show, Eq, Generic, AdditiveGroup, VectorSpace)
makeLenses ''DState


-- Connects State and DState into an AffineSpace/VectorSpace structure.
instance (AdditiveGroup a, Num a) => AffineSpace (State a) where
  type Diff (State a) = DState a
  s1 .-. s2 = DState
              { _dpos = s1^.pos |-| s2^.pos
              , _dvel = s1^.vel |-| s2^.vel
              }
  s .+^ ds = State
             { _pos = s^.pos |+| ds^.dpos
             , _vel = s^.vel |+| ds^.dvel
             }


plotEulerSHM :: Plot.Output -> IO ()
plotEulerSHM out = do
  let
    -- parameters
    ti =    0.0 % [si| ms |]     -- initial simulation time
    tf = 1500.0 % [si| ms |]     -- final simulation time
    x0 =   10.0 % [si| mm |]     -- initial position
    v0 =    0.0 % [si| m/s |]    -- initial velocity
    k  =   10.0 % [si| mN/mm |]  -- spring stiffness
    m  =  500.0 % [si| g |]      -- mass
    omega = qSqrt (k |/| m)      -- angular frequency

    -- analytical solution
    analytic :: [U.Time Double] -> [(U.Time Double, U.Length Double)]
    analytic times = [ (t, x0 |*| cos (t |*| omega)) | t <- times ]

    -- numerical solution
    numerical :: [U.Time Double] -> [(U.Time Double, U.Length Double)]
    numerical times =
      let
        tne = NonEmpty.fromList times

        f :: (U.Time Double, State Double) -> (U.Time Double :-* DState Double)
        f (_, state) = linear
                       $ \(dt :: U.Time Double) ->
                           DState
                           { _dpos = state^.vel |*| dt
                           , _dvel = qNegate(state^.pos |*| k |/| m) |*| dt
                           }
        state0 = State { _pos = x0, _vel = v0 }
        timeAndPos r = (r^._1, r^._2.pos)
      in
        NonEmpty.toList
        $ timeAndPos <$> ODE.integrate ODE.eulerStep state0 tne f

  Plot.plotXYChartUnits
    out
    "Simple Harmonic Motion - Analytic vs Euler"
    "Time (ms)"
    "Position (mm)"
    ( [si| ms |], [si| mm |] )
    [ (Plot.Line "Analytic Solution" $ analytic (ODE.linspace 200 ti tf))
    , (Plot.Points "Euler (dt=75.0 ms)" $ numerical (ODE.linspace 20 ti tf))
    , (Plot.Points "Euler (dt=37.5 ms)" $ numerical (ODE.linspace 40 ti tf))
    , (Plot.Points "Euler (dt=7.5 ms)" $ numerical (ODE.linspace 200 ti tf))
    ]


plotSHMComparison :: Plot.Output -> IO ()
plotSHMComparison out = undefined {- do
  let
    analytic :: [Double] -> [(Double, Double)]
    analytic times = [ (t, cos (t * (sqrt 0.2))) | t <- times ]

    numeric
      :: ODE.Stepper Double (State Double) (DState Double)
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
  -}
