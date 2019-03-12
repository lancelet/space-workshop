{-|
Module      : LunarAscent.Types
Description : Types used for the lunar ascent simulation.
-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DataKinds          #-}
module LunarAscent.Types
  ( -- * Units (type aliases)
    R
  , V2
  , DimensionlessV2
  , Length
  , Position
  , Velocity
  , Acceleration
  , Mass
  , Time
  , MassFlowRate
  , ThrustAngle(..)
    -- * Types
  , AscentTarget(..)
  , EngineState(..)
  , EngineCutoffCall(..)
  , PositionControl(..)
  , AscentPhase(..)
  , Flags
  , DynState
  , DDynState
  , LunarModuleSim
    -- * Lenses
  , targetVelocity
  , targetAltitude
  , engineState
  , engineCutoffCall
  , positionControl
  , ascentPhase
  , pos
  , vel
  , mass
  , posDot
  , velDot
  , massDot
  , flags
  , time
  , tgo
  , dynState
    -- * Functions
  , mkFlags
  , initialFlags
  , mkDynState
  , mkDynStateGradient
  , mkLunarModuleSim
  ) where

import           Control.Lens           (Lens', lens, makeLenses)
import           Data.AdditiveGroup     (AdditiveGroup, (^+^), (^-^))
import           Data.AffineSpace       (AffineSpace, Diff, (.+^), (.-.))
import           Data.Metrology         (type (%/), (:/) ((:/)), (:@) ((:@)), Qu)
import           Data.Metrology.Show    ()
import           Data.Metrology.SI.Poly (Gram (Gram), Kilo (Kilo),
                                         Meter (Meter), SI, Second (Second))
import qualified Data.Metrology.SI.Poly as SIPoly
import           Data.Metrology.Vector  (( # ), (%))
import           Data.VectorSpace       (VectorSpace)
import           GHC.Generics           (Generic)
import qualified Linear
import           Orphans                ()

type R = Double
type V2 = Linear.V2 R
type DimensionlessV2 = Qu '[] SI V2
type Length = SIPoly.Length SI Double
type Position = SIPoly.Length SI V2
type Velocity = SIPoly.Velocity SI V2
type Acceleration = SIPoly.Acceleration SI V2
type Mass = SIPoly.Mass SI R
type Time = SIPoly.Time SI R
type MassFlowRate = Mass %/ Time
newtype ThrustAngle = ThrustAngle R


data AscentTarget
  = AscentTarget
    { _targetVelocity :: Velocity
    , _targetAltitude :: Length
    } deriving (Show)
makeLenses ''AscentTarget


data EngineState
  = Ignited
  | Cutoff
  deriving (Show)

data EngineCutoffCall
  = EngineContinue
  | CutoffCallMade
  deriving (Show)

data PositionControl
  = PositionControlInactive
  | PositionControlActive
  deriving (Show)

data AscentPhase
  = VerticalRise
  | ControlledAscent
  deriving (Show)

data Flags
  = Flags
    { _engineState      :: !EngineState       -- ^ Coasting-only
    , _engineCutoffCall :: !EngineCutoffCall  -- ^ @FLENG2@ in the AGC
    , _positionControl  :: !PositionControl   -- ^ @FLPC@ in the AGC
    , _ascentPhase      :: !AscentPhase       -- ^ @FLVP@ in the AGC
    } deriving (Show)
makeLenses ''Flags

instance Semigroup Flags where
  _ <> x = x

instance Monoid Flags where
  mempty = initialFlags

mkFlags
  :: EngineState
  -> EngineCutoffCall
  -> PositionControl
  -> AscentPhase
  -> Flags
mkFlags = Flags


initialFlags :: Flags
initialFlags
  = mkFlags Ignited EngineContinue PositionControlInactive VerticalRise


-- | Dynamical variables in the lunar module state.
data DynState
  = DynState
    { _pos  :: V2         -- m
    , _vel  :: V2         -- m/2
    , _mass :: Double     -- kg
    } deriving (Show)

mkDynState :: Position -> Velocity -> Mass -> DynState
mkDynState p v m
  = DynState
    { _pos  = p # Meter
    , _vel  = v # Meter :/ Second
    , _mass = m # Kilo :@ Gram
    }

pos :: Lens' DynState Position
pos = lens (\ds -> (_pos ds) % Meter)
           (\ds pos' -> ds {_pos = pos' # Meter})

vel :: Lens' DynState Velocity
vel =  lens (\ds -> (_vel ds) % Meter :/ Second)
            (\ds vel' -> ds {_vel = vel' # Meter :/ Second})

mass :: Lens' DynState Mass
mass = lens (\ds -> (_mass ds) % Kilo :@ Gram)
            (\ds mass' -> ds {_mass = mass' # Kilo :@ Gram})


-- | Delta of the lunar module dynamical state.
data DDynState
  = DDynState
    { _dpos  :: V2         -- m (delta) or m/s (gradient)
    , _dvel  :: V2         -- m/s (delta) or m/s^2 (gradient)
    , _dmass :: Double     -- kg (delta) or kg/s (gradient)
    } deriving (Show, Generic, AdditiveGroup, VectorSpace)

mkDynStateGradient :: Velocity -> Acceleration -> MassFlowRate -> DDynState
mkDynStateGradient v a mdot
  = DDynState
    { _dpos = v # Meter :/ Second
    , _dvel = a # Meter :/ Second :/ Second
    , _dmass = mdot # Kilo :@ Gram :/ Second
    }

posDot :: Lens' DDynState Velocity
posDot = lens (\dds -> (_dpos dds) % Meter :/ Second)
              (\dds posDot' -> dds{_dpos = posDot' # Meter :/ Second})

velDot :: Lens' DDynState Acceleration
velDot
  = lens
    (\dds -> (_dvel dds) % Meter :/ Second :/ Second)
    (\dds velDot' -> dds{_dvel = velDot' # Meter :/ Second :/ Second})

massDot :: Lens' DDynState MassFlowRate
massDot
  = lens
    (\dds -> (_dmass dds) % Kilo :@ Gram :/ Second)
    (\dds massDot' -> dds{_dmass = massDot' # Kilo :@ Gram :/ Second})

-- | Lunar module simulation state.
data LunarModuleSim
  = LunarModuleSim
    { _flags    :: Flags
    , _time     :: Time
    , _tgo      :: Time
    , _dynState :: DynState
    } deriving (Show)
makeLenses ''LunarModuleSim

instance AffineSpace DynState where
  type Diff DynState = DDynState
  s1 .-. s2 = DDynState
              { _dpos = _pos s1 ^-^ _pos s2
              , _dvel = _vel s1 ^-^ _vel s2
              , _dmass = _mass s1 ^-^ _mass s2
              }
  s .+^ ds = DynState
             { _pos = _pos s ^+^ _dpos ds
             , _vel = _vel s ^+^ _dvel ds
             , _mass = _mass s ^+^ _dmass ds
             }

mkLunarModuleSim :: Flags -> Time -> Time -> DynState -> LunarModuleSim
mkLunarModuleSim f t tgo' dstate
  = LunarModuleSim
    { _flags = f
    , _time = t
    , _tgo = tgo'
    , _dynState = dstate
    }
