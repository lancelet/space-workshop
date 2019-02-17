{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
module LunarAscent where

import           Data.List.NonEmpty   (NonEmpty ((:|)))
import qualified Data.List.NonEmpty   as NonEmpty
import           Diagrams.Backend.SVG (B)
import qualified Diagrams.Backend.SVG as SVG
import           Diagrams.Prelude     (Diagram, ( # ))
import qualified Diagrams.Prelude     as D
import           Linear               ((*^), (^+^))
import           Linear.Metric        (norm, normalize)
import           Linear.V2            (V2 (V2), _x, _y)
import Control.Lens ((^.))

import qualified ODE.FixedStep        as ODE

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

data State
  = State
    { mass :: !Double
    , px   :: !Double
    , py   :: !Double
    , vx   :: !Double
    , vy   :: !Double
    } deriving (Show)


lmAscentStageDryMass :: Double
lmAscentStageDryMass = 2445.0  -- kg

lmAscentStageFuelBurnMass :: Double
lmAscentStageFuelBurnMass = 2252.0  -- kg

stateInit :: State
stateInit
  = State
    { mass = lmAscentStageDryMass + lmAscentStageFuelBurnMass + 250.0 -- extra 250kg?
    , px = 0.0
    , py = lunarRadius * 1000.0 -- m
    , vx = 0.0
    , vy = 0.0
    }


stateOps :: ODE.Ops State Double
stateOps
  = ODE.Ops
    { ODE.scalarMul = \c state ->
        State
        { mass = c * mass state
        , px = c * px state
        , py = c * py state
        , vx = c * vx state
        , vy = c * vy state
        }
    , ODE.addVec = \s1 s2 ->
        State
        { mass = mass s1 + mass s2
        , px = px s1 + px s2
        , py = py s1 + py s2
        , vx = vx s1 + vx s2
        , vy = vy s1 + vy s2
        }
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

eom :: Double -> Double -> Double -> State -> State
eom mdot thrust time state =
  let
    p = V2 (px state) (py state)
    m = mass state

    -- radius of the vehicle
    r = norm p

    -- force due to gravity
    fGMag = gravitationalConstant * lunarMass * m / (r*r)
    fG = fGMag *^ (normalize (-p))

    -- force due to thrust
    dirP = (pi / 2.0) - atan2 (py state) (px state)
    dir = dirP + ((attitudeGuidance time) * pi / 180.0)
    -- dir = (attitudeGuidance time) * pi / 180.0
    phi = (pi / 2.0) - dir
    fT = thrust *^ V2 (cos phi) (sin phi)

    -- total force
    f = fG ^+^ fT

    -- acceleration
    a = (1.0 / m) *^ f

    -- mdot = - lmAscentStageFuelBurnMass / 438.0  -- kg / s
    pxdot = vx state
    pydot = vy state
    vxdot = a ^. _x
    vydot = a ^. _y
  in
    State
    { mass = mdot
    , px = pxdot
    , py = pydot
    , vx = vxdot
    , vy = vydot
    }


ascent :: [(Double, State)]
ascent =
  let
    integrator = ODE.integrate (ODE.rk4Step stateOps)
    steps = 0.0 :| [1.0, 2.0 .. 438.0 ]
    -- steps = 0.0 :| [ 1.0, 2.0 .. 60.0 ]
  in
    NonEmpty.toList $ integrator stateInit steps
                      (eom (-lmAscentStageFuelBurnMass / 438.0) lmAscentStageThrust)


orbit :: [(Double, State)]
orbit =
  let
    integrator = ODE.integrate (ODE.rk4Step stateOps)
    steps = 438.0 :| [450.0, 500.0 .. 10000.0 ]
    state0 = snd . last $ ascent
  in
    NonEmpty.toList $ integrator state0 steps (eom 0 0)


fancyOrbitScale :: (Double, Double) -> (Double, Double)
fancyOrbitScale (x, y) = (x', y')
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
    pos = fmap (\(_, s) -> D.p2 . fancyOrbitScale $ (px s / 1000.0, py s / 1000.0)) ascent
  in
    D.fromVertices pos
    # D.lc D.red
  

orbitDia :: Diagram B
orbitDia =
  let
    pos = fmap (\(_, s) -> D.p2 . fancyOrbitScale $ (px s / 1000.0, py s / 1000.0)) orbit
  in
    D.fromVertices pos
    # D.lc D.green


renderDiagram :: IO ()
renderDiagram =
  let
    dia = D.frame 100.0 $ ascentDia <> orbitDia <> csmOrbit <> moon
    endBurn = snd . last $ ascent
    pEndBurn = V2 (px endBurn) (py endBurn)
    rEndBurn = norm pEndBurn
    altitudeEndBurn = rEndBurn - (lunarRadius * 1000.0)
  in do
    putStrLn $ "Altitute at end of burn (m): " <> show altitudeEndBurn
    SVG.renderSVG "lunar-ascent.svg" (D.mkWidth 800) dia
