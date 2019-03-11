{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module LunarAscent.Types
  ( -- * Types
    EngineState(..)
  , EngineCutoffCall(..)
  , PositionControl(..)
  , AscentPhase(..)
  , Flags
  , DynState
  , DDynState
  , LunarModuleSim
    -- * Lenses
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
  , dynState
    -- * Functions
  , mkFlags
  , initialFlags
  ) where

import           Control.Lens           (Lens', lens, makeLenses, (^.))
import           Data.AdditiveGroup     (AdditiveGroup, (^+^), (^-^))
import           Data.AffineSpace       (AffineSpace, Diff, (.+^), (.-.))
import           Data.Metrology         ((:/) ((:/)), (:@) ((:@)), type (%/))
import           Data.Metrology.SI.Poly (Gram (Gram), Kilo (Kilo),
                                         Meter (Meter), SI, Second (Second))
import qualified Data.Metrology.SI.Poly as SIPoly
import           Data.Metrology.Vector  (( # ), (%))
import           Data.VectorSpace       (VectorSpace)
import           GHC.Generics           (Generic)
import qualified Linear
import           Orphans                ()


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


type R = Double
type V2 = Linear.V2 R
type Position = SIPoly.Length SI V2
type Velocity = SIPoly.Velocity SI V2
type Acceleration = SIPoly.Acceleration SI V2
type Mass = SIPoly.Mass SI R
type Time = SIPoly.Time SI R

-- | Dynamical variables in the lunar module state.
data DynState
  = DynState
    { _pos  :: V2         -- m
    , _vel  :: V2         -- m/2
    , _mass :: Double     -- kg
    } deriving (Show)

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

posDot :: Lens' DDynState Velocity
posDot = lens (\dds -> (_dpos dds) % Meter :/ Second)
              (\dds posDot' -> dds{_dpos = posDot' # Meter :/ Second})

velDot :: Lens' DDynState Acceleration
velDot
  = lens
    (\dds -> (_dvel dds) % Meter :/ Second :/ Second)
    (\dds velDot' -> dds{_dvel = velDot' # Meter :/ Second :/ Second})

massDot :: Lens' DDynState (Mass %/ Time)
massDot
  = lens
    (\dds -> (_dmass dds) % Kilo :@ Gram :/ Second)
    (\dds massDot' -> dds{_dmass = massDot' # Kilo :@ Gram :/ Second})

-- | Lunar module simulation state.
data LunarModuleSim
  = LunarModuleSim
    { _flags    :: Flags
    , _time     :: Double
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
