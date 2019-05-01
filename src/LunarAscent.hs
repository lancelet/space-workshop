{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module LunarAscent where

import           Control.Lens       ((<&>), (^.))
import           Data.Basis         (Basis, HasBasis)
import           Data.LinearMap     ((:-*), linear)
import           Data.List          (unfoldr)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.VectorSpace   (InnerSpace, Scalar, VectorSpace, zeroV)
import qualified Diagrams.Prelude   as D

import           LunarAscent.AGC    (gravAccel, p12)
import           LunarAscent.Types  (AGCState (AGCState, _prevThrustAngle, _tgo),
                                     AscentTarget (AscentTarget), Constants,
                                     DDynamics (DDynamics, _dangle, _dangvel, _dmass, _dpos, _dvel),
                                     Dynamics (Dynamics, _angle, _angvel, _mass, _pos, _vel),
                                     EngineShutoff (EngineShutoff), MFCS, P2,
                                     Sim (Sim, _agcState, _ddynamics, _dynamics, _engineShutoff, _time),
                                     ThrustAngle (ThrustAngle), agcState, angle,
                                     angvel, apsExhaustVelocity,
                                     apsMassFlowRate, commandedEngineShutoff,
                                     commandedThrustAngle, dynamics,
                                     engineShutoff, initialMass, mass, moonSGP,
                                     pos, qv2, time, v2, v2Tuple, vel)
import qualified LunarAscent.Types  as Types
import qualified ODE
import qualified Plot
import           Units              (si, ( # ), (%), (%.), (*|), (.#), (|*^|),
                                     (|*|), (|+|), (|-|), (|.-.|), (|.|), (|/|),
                                     (|^*|))
import qualified Units              as U


defaultAscentTarget :: AscentTarget Double
defaultAscentTarget
  = AscentTarget
    (v2 5.9436 1679.2956 % [si| m/s |])
    ((1731.1 % [si| km |]) |+| (18288.0 % [si| m |]))


plotLunarAscentMoonView :: Plot.Output -> IO ()
plotLunarAscentMoonView output = do
  let
    burnSteps = burn BurnLastOnly 10 (Types.constants) defaultAscentTarget
    coastSteps = coast (Types.constants) (last burnSteps)
    rBurn = (eachN 10 burnSteps) <&> (\sim -> sim^.dynamics^.pos .# [si| km |]) <&> v2Tuple
    rCoast = coastSteps <&> (\sim -> sim^.dynamics^.pos .# [si| km |]) <&> v2Tuple
  Plot.plotOrbitSystem output
    (Plot.OrbitSystem
     [ Plot.Trajectory "Burn" (0,1) rBurn D.red
     , Plot.Trajectory "Coast" (1,1) rCoast D.green
     , Plot.Planet "Moon" 1731.1 D.grey
     ])


plotLunarAscentVerticalRise :: Plot.Output -> IO ()
plotLunarAscentVerticalRise output = do
  let
    burnSteps = burn BurnIncludeAll 10 (Types.constants) defaultAscentTarget
    initRise = takeWhile (\sim -> sim^.time <= 16.0 % [si| s |]) burnSteps
    rInitRise = initRise
            <&> (\sim -> sim^.dynamics.pos .# [si| km |])
            <&> v2Tuple
            <&> toRangeCoords
            <&> (\(x, y) -> (x * 1000.0, y * 1000.0))
    twoSecondFlags = eachN 10 rInitRise
  Plot.xyChart
    output
    "Vertical Rise Phase (16 seconds)"
    "Down Range (m)"
    "Altitude (m)"
    [ Plot.Line "Ascent Trajectory" rInitRise
    , Plot.Points "2s markers (one every 2s)" twoSecondFlags
    ]


plotLunarAscentBurnOnly :: Plot.Output -> IO ()
plotLunarAscentBurnOnly output = do
  let
    burnSteps = burn BurnLastOnly 10 (Types.constants) defaultAscentTarget
    rBurn = burnSteps <&> (\sim -> sim^.dynamics.pos .# [si| km |]) <&> v2Tuple <&> toRangeCoords
  Plot.xyChart
    output
    "Lunar Ascent Burn Trajectory"
    "Down Range (km)"
    "Altitude (km)"
    [ Plot.Line "Burn Trajectory" rBurn
    ]

-- | Convert MFCS to range coordinates (Down-range, altitude); units of km.
toRangeCoords :: (Double, Double) -> (Double, Double)
toRangeCoords (xmfcs, ymfcs) = (downRange, altitude)
  where
    altitude  = sqrt (xmfcs*xmfcs + ymfcs*ymfcs) - 1731.1
    downRange = 1731.1 * atan2 xmfcs ymfcs


-- | Which points to keep from the burn phase.
--
-- It turns out that `diagrams` just isn't plotting properly if all the points
-- are kept, so this lets us switch depending on the detail required.
data BurnInterp
  = BurnIncludeAll  -- ^ Include all interpolated points during the burn phase
  | BurnLastOnly    -- ^ Include only the last point from each 2s burn phase


burn
  :: forall a.
     ( InnerSpace a, a ~ Scalar a, RealFloat a, Ord a, Basis a ~ (), HasBasis a, Show a )
  => BurnInterp
  -> Int
  -> Constants a
  -> AscentTarget a
  -> [Sim a]
burn burnInterp nSteps constants target =
  let
    step :: Sim a -> Maybe (NonEmpty (Sim a), Sim a)
    step s =
      let
        rs = burnStep nSteps constants target s
      in
        rs <&> \ne -> (ne, NonEmpty.last ne)
  in
    case burnInterp of
      BurnLastOnly   -> NonEmpty.head <$> unfoldr step (initSim constants)
      BurnIncludeAll -> concat $ NonEmpty.toList <$> unfoldr step (initSim constants)


-- | Take every nth item from a list, starting from the first.
eachN
  :: Int  -- ^ how many to drop
  -> [a]  -- ^ input list of items
  -> [a]  -- ^ output list of items
eachN _ []     = []
eachN n (x:xs) = x : eachN n (drop (n - 1) xs)


coast
  :: forall a.
     ( InnerSpace a, a ~ Scalar a, RealFloat a, Ord a, Basis a ~ (), HasBasis a, Show a )
  => Constants a
  -> Sim a
  -> [Sim a]
coast constants sim =
  let
    f = gradFnNoThrust constants
    times = NonEmpty.fromList
          $ ODE.linspace 200 (sim^.time) (sim^.time |+| (10000 % [si| s |]))
    dynStates = NonEmpty.toList
              $ ODE.integrateWithDiff ODE.rk4Step (sim^.dynamics) times f

    mkSim (t, dyn, ddyn)
      = Sim
        { _time          = t
        , _engineShutoff = sim^.engineShutoff
        , _agcState      = sim^.agcState
        , _dynamics      = dyn
        , _ddynamics     = ddyn
        }
  in
    mkSim <$> dynStates


burnStep
  :: forall a.
     ( InnerSpace a, a ~ Scalar a, RealFloat a, Ord a, Basis a ~ (), HasBasis a, Show a )
  => Int                       -- number of time steps in the 2 second period
  -> Constants a               -- simulation constants
  -> AscentTarget a            -- ascent target
  -> Sim a                     -- starting simulation state
  -> Maybe (NonEmpty (Sim a))  -- produced simulation time steps
burnStep nSteps constants target sim =
  let
    -- consult the guidance computer
    (command, state) = p12 constants target sim

    -- figure out the ODE to solve for our current state
    cmdAngle = thrustAngleToGlobal (command^.commandedThrustAngle) (sim^.dynamics^.pos)
    angAccel = constAngAccel
               (2 % [si| s |])
               (sim^.dynamics^.angle)
               cmdAngle
               (sim^.dynamics^.angvel)
    f = gradFn constants angAccel (command^.commandedEngineShutoff)

    -- integrate the ODE over the 2-second period with constant control
    times = NonEmpty.fromList
          $ ODE.linspace nSteps (sim^.time) (sim^.time |+| (2 % [si| s |]))
    dynStates = ODE.integrateWithDiff ODE.rk4Step (sim^.dynamics) times f

    -- mkSim converts the output of each integration step to a Sim
    mkSim
      :: (U.Time a, Dynamics a, U.Time a :-* DDynamics a)
      -> Sim a
    mkSim (t, dyn, ddyn)
      = Sim
        { _time          = t
        , _engineShutoff = sim^.engineShutoff
                        <> command^.commandedEngineShutoff
        , _agcState      = state
        , _dynamics      = dyn
        , _ddynamics     = ddyn
        }

    -- check if we're actually after the engine shutoff time
    afterShutoff
      = maybe
        False
        (\(EngineShutoff tshut) -> tshut <= sim^.time)
        (sim^.engineShutoff)
  in
    if afterShutoff
    then Nothing
    else Just (mkSim <$> dynStates)


gradFn
  :: ( InnerSpace a, a ~ Scalar a, HasBasis a, Basis a ~ (), Floating a, Ord a, Show a )
  => Constants a
  -> U.AngAcceleration a       -- ^ commanded angular acceleration
  -> Maybe (EngineShutoff a)   -- ^ possible engine shutoff time
  -> (U.Time a, Dynamics a)    -- ^ time and system dynamic state
  -> U.Time a :-* DDynamics a  -- ^ gradient of dynamic state
gradFn constants angAccel shutoff (t, dyn) =
  let
    phi = dyn^.angle # [si| |]
    vhat = qv2 (cos phi) (sin phi)
    thrustMag = ( (constants^.apsExhaustVelocity # [si| m/s  |])
                * (constants^.apsMassFlowRate    # [si| kg/s |])
                ) % [si| N |]  -- the type checker just fails on this one

    -- only apply thrust if the time is prior to an engine shutoff command
    thrust = if (maybe True (\(EngineShutoff tshut) -> t < tshut) shutoff)
             then thrustMag |/| dyn^.mass |*^| vhat
             else zeroV % [si| m/s^2 |]
    gravity = gravAccel (constants^.moonSGP) (dyn^.pos)
  in
    linear
    $ \dt ->
        DDynamics
        { _dpos    = dyn^.vel |^*| dt
        , _dvel    = (thrust |+| gravity) |^*| dt
        , _dmass   = -1 *| constants^.apsMassFlowRate |*| dt
        , _dangle  = dyn^.angvel |*| dt
        , _dangvel = angAccel |*| dt
        }


gradFnNoThrust
  :: ( InnerSpace a, a ~ Scalar a, HasBasis a, Basis a ~ (), Floating a, Ord a, Show a )
  => Constants a
  -> (U.Time a, Dynamics a)    -- ^ time and system dynamic state
  -> U.Time a :-* DDynamics a  -- ^ gradient of dynamic state
gradFnNoThrust constants (_, dyn) =
  let
    gravity = gravAccel (constants^.moonSGP) (dyn^.pos)
  in
    linear
    $ \dt ->
        DDynamics
        { _dpos    = dyn^.vel |^*| dt
        , _dvel    = gravity |^*| dt
        , _dmass   = 0 % [si| kg |]
        , _dangle  = dyn^.angvel |*| dt
        , _dangvel = 0 % [si| 1/s |]
        }


thrustAngleToGlobal
  :: ( InnerSpace a, a ~ Scalar a, RealFloat a )
  => ThrustAngle a
  -> U.Length (P2 MFCS a)
  -> U.Count a
thrustAngleToGlobal (ThrustAngle x) p =
  let
    pv = p |.-.| (zeroV %. [si| m |])
    px = (pv |.| qv2 1 0) # [si| m |]
    py = (pv |.| qv2 0 1) # [si| m |]
    pangle = atan2 py px
  in
    U.quantity (pangle - x)


constAngAccel
  :: ( VectorSpace a, a ~ Scalar a, Fractional a )
  => U.Time a             -- ^ time delta
  -> U.Count a            -- ^ current angle
  -> U.Count a            -- ^ desired angle
  -> U.AngVelocity a      -- ^ current angular velocity
  -> U.AngAcceleration a  -- ^ required (constant) angular acceleration
constAngAccel dt theta_i theta_f omega
  = 2 *| ((theta_f |-| theta_i) |/| dt |-| omega) |/| dt


-- | Initial simulation state.
initSim
  :: forall a.
     ( HasBasis a, Basis a ~ (), Floating a, InnerSpace a, a ~ Scalar a )
  => Constants a
  -> Sim a
initSim constants
  = Sim
    { _time = 0 % [si| s |]
    , _engineShutoff = Nothing
    , _agcState = AGCState
                  { _tgo             = 370 % [si| s |]
                  , _prevThrustAngle = ThrustAngle 0
                  }
    , _dynamics = Dynamics
                  { _pos    = (v2 0 1731.1) %. [si| km |]
                  , _vel    = zeroV % [si| m/s |]
                  , _mass   = constants^.initialMass
                  , _angle  = U.quantity (pi/2)
                  , _angvel = 0 % [si| 1/s |]
                  }
    , _ddynamics = linear
                   $ \(dt :: U.Time a) ->
                       DDynamics
                       { _dpos    = (zeroV % [si| m/s |]) |^*| dt
                       , _dvel    = (zeroV % [si| m/s^2 |]) |^*| dt
                       , _dmass   = -1 *| constants^.apsMassFlowRate |*| dt
                       , _dangle  = U.quantity 0
                       , _dangvel = 0 % [si| 1/s |]
                       }
    }
