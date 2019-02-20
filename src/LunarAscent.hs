{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
module LunarAscent where

import           Control.Lens          (makeLenses, (^.))
import           Data.AdditiveGroup    (AdditiveGroup)
import           Data.AffineSpace      (AffineSpace, Diff, (.+^), (.-.))
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty    as NonEmpty
import           Data.VectorSpace      (VectorSpace, (^/))
import           Data.VectorSpace.Free ()
import           Diagrams.Backend.SVG  (B)
import qualified Diagrams.Backend.SVG  as SVG
import           Diagrams.Prelude      (Diagram, ( # ))
import qualified Diagrams.Prelude      as D
import           GHC.Generics          (Generic)
import           Linear                ((*^), (^+^))
import           Linear.Metric         (norm, normalize)
import           Linear.V2             (V2 (V2), _x, _y)

import qualified ODE.FixedStepV        as ODE

{-
Apollo parameters are taken from:

Bennett, Floyd V (1970) Apollo Lunar Descent and Ascent Trajectories.
  Presented at the AIAA 8th Aerospace Sciences Meeting,
  New York, New York, January 19--21, 1970.
  NASA Technical Memorandum TM X--58040, March 1970.

  URL: https://www.hq.nasa.gov/alsj/nasa58040.pdf
-}


lunarRadius :: Double
lunarRadius = 1737.1  -- km


csmOrbitHeight :: Double
csmOrbitHeight = 111.12  -- km (= 60 NM) above the lunar surface


moon :: Diagram B
moon
  = D.circle lunarRadius
  # D.fc D.lightgray


csmOrbit :: Diagram B
csmOrbit
  = D.circle ((8 * csmOrbitHeight) + lunarRadius)  -- additional scaling here
  # D.lc D.gray


data DState
  = DState
    { _dmass :: !Double
    , _dpos  :: !(V2 Double)
    , _dvel  :: !(V2 Double)
    } deriving (Generic, AdditiveGroup, VectorSpace, Show)
makeLenses ''DState

data State
  = State
    { _mass :: !Double
    , _pos  :: !(V2 Double)
    , _vel  :: !(V2 Double)
    } deriving (Generic, Show)
makeLenses ''State

instance AffineSpace State where
  type Diff State = DState
  s1 .-. s2 = DState
              { _dmass = s1 ^. mass - s2 ^. mass
              , _dpos  = s1 ^. pos  - s2 ^. pos
              , _dvel  = s1 ^. vel  - s2 ^. vel
              }
  s .+^ ds = State
             { _mass = s ^. mass + ds ^. dmass
             , _pos  = s ^. pos  + ds ^. dpos
             , _vel  = s ^. vel  + ds ^. dvel
             }


lmAscentStageDryMass :: Double
lmAscentStageDryMass = 2445.0  -- kg

lmAscentStageFuelBurnMass :: Double
lmAscentStageFuelBurnMass = 2252.0  -- kg

stateInit :: State
stateInit
  = State
    { _mass = lmAscentStageDryMass + lmAscentStageFuelBurnMass + 250.0 -- extra 250kg?
    , _pos = V2 0 (lunarRadius * 1000)  -- m
    , _vel = V2 0 0
    }


gravitationalConstant :: Double
gravitationalConstant = 6.6740831E-11  -- m^3 / kg / s


lunarMass :: Double
lunarMass = 7.342E22  -- kg


attitudeGuidance :: Double -> Double
attitudeGuidance time | time < 10 = 0
                      -- | otherwise = (time - 8.6) * 7.0
                      | time < 16 = (time - 8.6) * 7.0
                      | otherwise = 52.0 + (time - 16.0) * 0.0932 {- 9.0E-2 -}

lmAscentStageThrust :: Double
lmAscentStageThrust = 15600.0  -- N

eom :: Double -> Double -> (Double, State) -> DState
eom mdot thrust (time, state) =
  let
    m = state ^. mass
    p = state ^. pos

    -- radius of the vehicle
    r = norm p

    -- force due to gravity
    fGMag = gravitationalConstant * lunarMass * m / (r*r)
    fG = fGMag *^ (normalize (-p))

    -- force due to thrust
    dirP = (pi / 2.0) - atan2 (p ^. _y) (p ^. _x)
    dir = dirP + ((attitudeGuidance time) * pi / 180.0)
    phi = (pi / 2.0) - dir
    fT = thrust *^ V2 (cos phi) (sin phi)

    -- total force
    f = fG ^+^ fT

    -- acceleration
    a = f ^/ m

  in
    DState
    { _dmass = mdot
    , _dpos  = state ^. vel
    , _dvel  = a
    }


ascent :: [(Double, State)]
ascent =
  let
    steps = 0.0 :| [1.0, 2.0 .. 438.0 ]
  in
    NonEmpty.toList
    $ ODE.integrate ODE.rk4Step stateInit steps
      (eom (-lmAscentStageFuelBurnMass / 438.0) lmAscentStageThrust)


orbit :: [(Double, State)]
orbit =
  let
    integrator = ODE.integrate ODE.rk4Step
    steps = 438.0 :| [450.0, 500.0 .. 10000.0 ]
    state0 = snd . last $ ascent
  in
    NonEmpty.toList $ integrator state0 steps (eom 0 0)


fancyOrbitScale :: V2 Double -> V2 Double
fancyOrbitScale (V2 x y) = V2 x' y'
  where
    scale = 8.0

    r = sqrt (x*x + y*y)
    theta = atan2 y x

    r' = scale * (r - lunarRadius) + lunarRadius
    x' = r' * cos theta
    y' = r' * sin theta


ascentDia :: Diagram B
ascentDia =
  let
    pp = fmap (\(_, s) -> D.P . fancyOrbitScale $ s ^. pos ^/ 1000) ascent
  in
    D.fromVertices pp
    # D.lc D.red


orbitDia :: Diagram B
orbitDia =
  let
    pp = fmap (\(_, s) -> D.P . fancyOrbitScale $ s ^. pos ^/ 1000.0) orbit
  in
    D.fromVertices pp
    # D.lc D.green


renderDiagram :: IO ()
renderDiagram =
  let
    dia = D.frame 100.0 $ ascentDia <> orbitDia <> csmOrbit <> moon
    endBurn = snd . last $ ascent
    pEndBurn = endBurn ^. pos
    rEndBurn = norm pEndBurn
    altitudeEndBurn = rEndBurn - (lunarRadius * 1000.0)
  in do
    putStrLn $ "Altitute at end of burn (m): " <> show altitudeEndBurn
    SVG.renderSVG "lunar-ascent.svg" (D.mkWidth 800) dia
