{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module LunarAscent2.Types
  ( -- ^ Coordinate systems
    MFCS(MFCS)
  , LVCS(LVCS)
    -- ^ Vectors and Points
  , V2(V2)
  , P2(P2)
    -- ^ Ascent stage state
  , ThrustAngle
  , Dynamics(Dynamics), pos, vel, mass
  , DDynamics(DDynamics), dpos, dvel, dmass
  , AGCState(AGCState), tgo
  , Sim(Sim), time, agcState, dynamics, dDynamics
    -- ^ ACG commands
  , EngineShutoff (EngineShutoff)
  , AGCCommand(AGCCommand), commandedThrustAngle, commandedEngineShutoff
    -- ^ Simulation constants
  , Constants(Constants), moonSGP, apsExhaustVelocity
  ) where

import           Control.Lens          (makeLenses, (^.))
import           Data.AdditiveGroup    (AdditiveGroup, negateV, zeroV, (^+^),
                                        (^-^))
import           Data.AffineSpace      (AffineSpace, Diff, (.+^), (.-.))
import           Data.LinearMap        ((:-*))
import           Data.Metrology.Show   ()
import           Data.Metrology.Vector ((*|), (|*^|), (|*|))
import           Data.VectorSpace      (Scalar, VectorSpace, (*^))
import           GHC.Generics          (Generic)

import qualified Units                 as U

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


-- | P2 is a 2D point in coordinate system @c@ with numeric type @a@.
--
-- Its associated vector space is 'V2'.
data P2 c a = P2 !a !a deriving (Show, Eq)

instance (VectorSpace a) => AffineSpace (P2 c a) where
  type Diff (P2 c a) = V2 c a
  (P2 x1 y1) .-. (P2 x2 y2) = V2 (x1 ^-^ x2) (y1 ^-^ y2)
  (P2 xp yp) .+^ (V2 xv yv) = P2 (xp ^+^ xv) (yp ^+^ yv)


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
    { _pos         :: !(U.Length (V2 MFCS a))
    , _vel         :: !(U.Length (V2 MFCS a))
    , _mass        :: !(U.Mass a)
    , _thrustAngle :: !(ThrustAngle a)
    } deriving (Show)
makeLenses ''Dynamics


-- | Delta in the dynamical state.
data DDynamics a
  = DDynamics
    { _dpos         :: !(U.Length (V2 MFCS a))
    , _dvel         :: !(U.Length (V2 MFCS a))
    , _dmass        :: !(U.Mass a)
    , _dThrustAngle :: !(ThrustAngle a)
    } deriving (Show, Generic, AdditiveGroup, VectorSpace)
makeLenses ''DDynamics

instance (VectorSpace a) => AffineSpace (Dynamics a) where
  type Diff (Dynamics a) = DDynamics a
  d1 .-. d2 = DDynamics
              { _dpos  = d1^.pos ^-^ d2^.pos
              , _dvel  = d1^.vel ^-^ d2^.vel
              , _dmass = d1^.mass ^-^ d2^.mass
              , _dThrustAngle = d1^.thrustAngle ^-^ d2^.thrustAngle
              }
  d .+^ dd = Dynamics
             { _pos  = d^.pos ^+^ dd^.dpos
             , _vel  = d^.vel ^+^ dd^.dvel
             , _mass = d^.mass ^+^ dd^.dmass
             , _thrustAngle = d^.thrustAngle ^+^ dd^.dThrustAngle
             }


-- | State of the guidance computer.
data AGCState a
  = AGCState
    { -- | Previous time-to-go estimate (prior to engine cutoff).
      _tgo :: U.Time a
    } deriving (Show)
makeLenses ''AGCState


-- | Total simulation state.
data Sim a
  = Sim
    { -- | Simulation time.
      _time      :: U.Time a
      -- | Relevant state of the Apollo Guidance Computer.
    , _agcState  :: AGCState a
      -- | Dynamical state of the vehicle.
    , _dynamics  :: Dynamics a
      -- | Gradient of the dynamical state of the vehicle.
    , _dDynamics :: U.Time a :-* DDynamics a
    }
makeLenses ''Sim


-------------------------------------------------------------------------------
-- Commands (from the guidance computer back to the simulation)
-------------------------------------------------------------------------------


-- | Possible engine shutoff command.
newtype EngineShutoff a = EngineShutoff (U.Time a) deriving (Show)
  

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


data Constants
  = Constants
    { _moonSGP            :: Int
    , _apsExhaustVelocity :: Int
    }
makeLenses ''Constants
