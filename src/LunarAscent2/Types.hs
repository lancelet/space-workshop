{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module LunarAscent2.Types
  ( -- * Coordinate systems
    MFCS(MFCS)
  , LVCS(LVCS)
    -- * Vectors and Points
  , V2, v2, qv2, v2Tuple
  , P2, p2
  , qcsAssignV2
    -- * Ascent target
  , AscentTarget(..), targetVel, targetRadius
    -- * Ascent stage state
  , ThrustAngle(..)
  , Dynamics(..), pos, vel, mass, angle, angvel
  , DDynamics(..), dpos, dvel, dmass, dangle, dangvel
  , AGCState(..), tgo, prevThrustAngle
  , Sim(..), time, engineShutoff, agcState, dynamics, ddynamics, accel
    -- * ACG commands
  , EngineShutoff(..)
  , AGCCommand(..), commandedThrustAngle, commandedEngineShutoff
    -- * Simulation constants
  , Constants(..), moonSGP, apsExhaustVelocity, apsMassFlowRate
  , initialMass, rDotFLVPEnd, tEngineThreshold, t2_HoldAll, t3_PositionControl
  , bThreshold, constants
  ) where

import           Control.Lens       (Contravariant, Optic', Profunctor,
                                     makeLenses, to, (^.))
import           Data.AdditiveGroup (AdditiveGroup, negateV, zeroV, (^+^),
                                     (^-^))
import           Data.AffineSpace   (AffineSpace, Diff, (.+^), (.-.))
import           Data.Basis         (Basis, HasBasis)
import           Data.Coerce        (coerce)
import           Data.LinearMap     ((:-*), lapply)
import           Data.VectorSpace   (InnerSpace, Scalar, VectorSpace, (*^),
                                     (<.>))
import           GHC.Generics       (Generic)

import           Orphans            ()
import           Units              ((:/) ((:/)), si, (%), (|*|), (|+|), (|-|),
                                     (|.+^|), (|.-.|), (|^/|))
import qualified Units              as U

-------------------------------------------------------------------------------
-- Vectors, points and coordinate systems
-------------------------------------------------------------------------------

-- | Moon-fixed coordinate system.
--
-- 2D coordinate system centred at the moon COM. Similar to that
-- described in section 5.1.4.5 of Levine (1971), but in 2D and
-- without any consideration of rotational axes.
data MFCS = MFCS


-- | Local vertical coordinate system.
--
-- Local vertical coordinate system of the launch vehicle. See section
-- 5.3.5.2 of Levine (1971).
data LVCS = LVCS


-- | V2 is a 2D vector in coordinate system @c@ with numeric type @a@.
--
-- The Apollo ascent stage algorithm as implemented here uses two main
-- coordinate systems, so for safety, we distinguish between vectors
-- expressed in each of them using the phantom type @c@.
data V2 c a = V2 !a !a deriving (Show, Eq)

instance (AdditiveGroup a) => AdditiveGroup (V2 c a) where
  zeroV = V2 zeroV zeroV
  (V2 x1 y1) ^+^ (V2 x2 y2) = V2 (x1 ^+^ x2) (y1 ^+^ y2)
  (V2 x1 y1) ^-^ (V2 x2 y2) = V2 (x1 ^-^ x2) (y1 ^-^ y2)
  negateV (V2 x y) = V2 (negateV x) (negateV y)

instance (VectorSpace a) => VectorSpace (V2 c a) where
  type Scalar (V2 c a) = Scalar a
  k *^ (V2 x y) = V2 (k*^x) (k*^y)

instance
  ( VectorSpace a
  , AdditiveGroup (Scalar a)
  , InnerSpace a
  ) => InnerSpace (V2 c a) where
  (V2 x1 y1) <.> (V2 x2 y2) = (x1<.>x2) ^+^ (y1<.>y2)


-- | Assign the coordinate system of a V2 with units.
qcsAssignV2 :: U.Qu d l (V2 c1 a) -> U.Qu d l (V2 c2 a)
qcsAssignV2 = coerce


-- | Construct a 'V2'.
v2 :: forall c a. a -> a -> V2 c a
v2 vx vy = V2 vx vy


-- | Construct a 'V2' as a dimensionless quantity.
qv2 :: forall c a l. a -> a -> U.Qu '[] l (V2 c a)
qv2 vx vy = U.quantity (v2 vx vy)


-- | Convert a 'V2' to a tuple.
v2Tuple :: forall c a. V2 c a -> (a, a)
v2Tuple (V2 vx vy) = (vx, vy)


-- | P2 is a 2D point in coordinate system @c@ with numeric type @a@.
type P2 c a = U.Point (V2 c a)


-- | Construct a 'P2'.  TODO: remove?
p2 :: forall c a. a -> a -> P2 c a
p2 px py = U.Point (V2 px py)


-------------------------------------------------------------------------------
-- Ascent target
-------------------------------------------------------------------------------


-- | Target of the lunar ascent control.
data AscentTarget a
  = AscentTarget
    { -- | Target velocity in local vertical coordinate system.
      _targetVel    :: !(U.Velocity (V2 LVCS a))
      -- | Target radius.
    , _targetRadius :: !(U.Length a)
    } deriving (Show)
makeLenses ''AscentTarget


-------------------------------------------------------------------------------
-- State of the ascent stage
-------------------------------------------------------------------------------


-- | Thrust angle.
newtype ThrustAngle a
  = ThrustAngle { unThrustAngle :: a }
  deriving (Show, Generic, AdditiveGroup, VectorSpace)


-- | Dynamical state of the ascent stage vehicle.
data Dynamics a
  = Dynamics
    { _pos    :: !(U.Length (P2 MFCS a))
    , _vel    :: !(U.Velocity (V2 MFCS a))
    , _mass   :: !(U.Mass a)
    , _angle  :: !(U.Count a)
    , _angvel :: !(U.AngVelocity a)
    } deriving (Show)
makeLenses ''Dynamics


-- | Delta of the dynamical state.
data DDynamics a
  = DDynamics
    { _dpos    :: !(U.Length (V2 MFCS a))
    , _dvel    :: !(U.Velocity (V2 MFCS a))
    , _dmass   :: !(U.Mass a)
    , _dangle  :: !(U.Count a)
    , _dangvel :: !(U.AngVelocity a)
    } deriving (Show, Generic, AdditiveGroup, VectorSpace)
makeLenses ''DDynamics

instance (VectorSpace a) => AffineSpace (Dynamics a) where
  type Diff (Dynamics a) = DDynamics a
  d1 .-. d2 = DDynamics
              { _dpos    = d1^.pos  |.-.| d2^.pos
              , _dvel    = d1^.vel    |-| d2^.vel
              , _dmass   = d1^.mass   |-| d2^.mass
              , _dangle  = d1^.angle  |-| d2^.angle
              , _dangvel = d1^.angvel |-| d2^.angvel
              }
  d .+^ dd = Dynamics
             { _pos    = d^.pos  |.+^| dd^.dpos
             , _vel    = d^.vel    |+| dd^.dvel
             , _mass   = d^.mass   |+| dd^.dmass
             , _angle  = d^.angle  |+| dd^.dangle
             , _angvel = d^.angvel |+| dd^.dangvel
             }


-- | State of the guidance computer.
data AGCState a
  = AGCState
    { -- | Previous time-to-go estimate (prior to engine cutoff).
      _tgo             :: U.Time a
      -- | Previous thrust angle.
    , _prevThrustAngle :: ThrustAngle a
    } deriving (Show)
makeLenses ''AGCState


-- | Engine shutoff time.
newtype EngineShutoff a
  = EngineShutoff { unEngineShutoff :: U.Time a }
  deriving (Show)

instance Semigroup (EngineShutoff a) where
  x <> _ = x  -- accept the first shutoff command


-- | Total simulation state.
data Sim a
  = Sim
    { -- | Simulation time.
      _time          :: !(U.Time a)
      -- | Time for engine shutoff (if known yet).
    , _engineShutoff :: !(Maybe (EngineShutoff a))
      -- | Relevant state of the Apollo Guidance Computer.
    , _agcState      :: !(AGCState a)
      -- | Dynamical state of the vehicle.
    , _dynamics      :: !(Dynamics a)
      -- | Gradient of the dynamical state of the vehicle.
    , _ddynamics     :: !(U.Time a :-* DDynamics a)
    }
makeLenses ''Sim

instance (Show a, a ~ Scalar a, Basis a ~ (), InnerSpace a, HasBasis a, Floating a) => Show (Sim a) where
  show sim = "Sim { _time          = " <> (show (sim^.time))          <> "\n"
          <> "    , _engineShutoff = " <> (show (sim^.engineShutoff)) <> "\n"
          <> "    , _agcState      = " <> (show (sim^.agcState))      <> "\n"
          <> "    , _dynamics      = " <> (show (sim^.dynamics))      <> "\n"
          <> "    , accel          = " <> (show (sim^.accel))         <> "}\n"


-- | Getter for the current acceleration from 'Sim'.
accel
  :: forall a p f.
     ( Profunctor p, Contravariant f
     , InnerSpace a, a ~ Scalar a, HasBasis a, Basis a ~ (), Floating a )
  => Optic' p f (Sim a) (U.Acceleration (V2 MFCS a))
accel = to $ \sim ->
  let
    tDelta = 1 % [si| s |]
    dd = lapply (sim^.ddynamics) tDelta
    dv = dd^.dvel
  in
    dv |^/| tDelta


-------------------------------------------------------------------------------
-- Commands (from the guidance computer back to the simulation)
-------------------------------------------------------------------------------


-- | Commands returned by the Apollo Guidance Computer.
data AGCCommand a
  = AGCCommand
    { _commandedThrustAngle   :: !(ThrustAngle a)
    , _commandedEngineShutoff :: !(Maybe (EngineShutoff a))
    } deriving (Show)
makeLenses ''AGCCommand


-------------------------------------------------------------------------------
-- Simulation constants
-------------------------------------------------------------------------------


data Constants a
  = Constants
    { -- | Specific gravitational parameter.
      _moonSGP            :: !(U.SGPUnit a)
      -- | APS exhaust velocity.
    , _apsExhaustVelocity :: !(U.Velocity a)
      -- | Mass flow rate of APS fuel.
    , _apsMassFlowRate    :: !(U.MassFlowRate a)
      -- | Initial total mass of the vehicle (wet mass).
    , _initialMass        :: !(U.Mass a)
      -- | Radial velocity triggering end of vertical rise phase.
    , _rDotFLVPEnd        :: !(U.Velocity a)
      -- | When time-to-go gets below this value, request the engine
      --   to cut-off.
    , _tEngineThreshold   :: !(U.Time a)
      -- | t2 time parameter: after time-to-go is less than this, hold
      -- all control parameters.
    , _t2_HoldAll         :: !(U.Time a)
      -- | t3 time parameter: after time-to-go is less than this, use
      -- position control only.
    , _t3_PositionControl :: !(U.Time a)
      -- | Minimum threshold for "B" rate constant.
    , _bThreshold         :: !(U.BThresholdUnit a)
    } deriving (Show)
makeLenses ''Constants


constants
  :: forall a.
     ( Fractional a, VectorSpace a, a ~ Scalar a )
  => Constants a
constants =
  let
    -- mdot is the APS mass flow rate
    mdot = 11.32 % (U.Pound :/ U.Second)
    -- tau0 is the initial mass / mass flow rate
    tau0 = 919.02 % [si| s |]
  in
    Constants
    { _moonSGP            = 0.4902778e13 % [si| m^3/s^2 |]
    , _apsExhaustVelocity = 3030 % [si| m/s |]
    , _apsMassFlowRate    = mdot
    , _initialMass        = tau0 |*| mdot
    , _rDotFLVPEnd        = 40 % U.Foot :/ U.Second
    , _tEngineThreshold   = 4 % [si| s |]
    , _t2_HoldAll         = 2 % [si| s |]
    , _t3_PositionControl = 10 % [si| s |]
    , _bThreshold         = -0.1 % U.Foot :/ U.Second :/ U.Second :/ U.Second
    }
