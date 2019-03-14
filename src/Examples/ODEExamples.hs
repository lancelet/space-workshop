{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Examples.ODEExamples where

import           Control.Lens          (makeLenses, (<&>), (^.))
import           Data.AdditiveGroup    (AdditiveGroup)
import           Data.AffineSpace      (AffineSpace, Diff, (.+^), (.-.))
import qualified Data.List.NonEmpty    as NonEmpty
import           Data.Metrology.Vector ((|+|), (|-|))
import           Data.VectorSpace      (VectorSpace)
import           GHC.Generics          (Generic)

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
  Plot.plotXYChart out $ Plot.XYChart
    (Plot.Title "Exponential Decay - Analytic vs Euler")
    (Plot.XLabel "Time (t) - no units assigned")
    (Plot.YLabel "Amount (x) - no units assigned")
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
plotEulerSHM out = undefined {- do
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
-}


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
