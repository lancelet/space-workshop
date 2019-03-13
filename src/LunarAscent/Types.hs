{-|
Module      : LunarAscent.Types
Description : Types used for the lunar ascent simulation.
-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module LunarAscent.Types
  ( -- * Units (type aliases)
    R
  , V2
  , DimensionlessV2
  , Length
  , Position
  , Velocity
  , VelMag
  , Acceleration
  , AccelMag
  , Mass
  , Time
  , MassFlowRate
  , ThrustAngle(..)
    -- * Types
  , AscentTarget(..)
  , AscentStage(..)
  , DynState
  , DDynState
  , TGo(..)
  , GravAccel(..)
  , AGCState
  , LunarModuleSim
    -- * Lenses
  , targetVelocity
  , targetAltitude
  , pos
  , vel
  , mass
  , posDot
  , velDot
  , massDot
  , stage
  , tgo
  , gPrev
  , time
  , agcState
  , dynState
  , dDynState
    -- * Functions
  , mkAGCState
  , mkDynState
  , mkDynStateGradient
  , mkLunarModuleSim
  ) where

import           Control.Lens           (Lens', lens, makeLenses)
import           Data.AdditiveGroup     (AdditiveGroup, (^+^), (^-^))
import           Data.AffineSpace       (AffineSpace, Diff, (.+^), (.-.))
import qualified Data.Dimensions.SI     as SIDims
import           Data.Metrology.Show    ()
import           Data.Metrology.SI.Poly (Gram (Gram), Kilo (Kilo),
                                         Meter (Meter), SI, Second (Second))
import qualified Data.Metrology.SI.Poly as SIPoly
import           Data.Metrology.Vector  ((:/) ((:/)), (:@) ((:@)), MkQu_DLN, Qu,
                                         ( # ), (%), Number(Number))
import           Data.VectorSpace       (VectorSpace)
import           GHC.Generics           (Generic)
import qualified Linear
import           Orphans                ()

type R            = Double
type V2           = Linear.V2 R
type DimensionlessV2 = Qu '[] SI V2
type Length       = SIPoly.Length       SI Double
type Position     = SIPoly.Length       SI V2
type Velocity     = SIPoly.Velocity     SI V2
type VelMag       = SIPoly.Velocity     SI R
type Acceleration = SIPoly.Acceleration SI V2
type AccelMag     = SIPoly.Acceleration SI R
type Jerk         = MkQu_DLN (SIDims.Acceleration :/ SIDims.Time) SI V2
type Mass         = SIPoly.Mass SI R
type Time         = SIPoly.Time SI R
type MassFlowRate = MkQu_DLN (SIDims.Mass :/ SIDims.Time) SI Double
newtype ThrustAngle = ThrustAngle R
  

data AscentTarget
  = AscentTarget
    { _targetVelocity :: Velocity
    , _targetAltitude :: Length
    } deriving (Show)
makeLenses ''AscentTarget


-- | Which part of the ascent stage are we in?
data AscentStage
  = VerticalRise              -- ^ FLVP=1, FLPC=0, FLENG2=0
  | MainBurn                  -- ^ FLVP=0, FLPC=0, FLENG2=0
  | InjectionPositionControl  -- ^ FLVP=0, FLPC=1, FLENG2=0/1
  | Coasting                  -- ^ FLVP=0, FLPC=1, FLENG2=1
  deriving (Show)

-- | Dynamical variables in the lunar module state.
data DynState
  = DynState
    { _pos   :: V2         -- m
    , _vel   :: V2         -- m/s
    , _accel :: V2         -- m/s/s
    , _mass  :: Double     -- kg
    } deriving (Show)

mkDynState :: Position -> Velocity -> Mass -> DynState
mkDynState p v m
  = DynState
    { _pos   = p # Meter
    , _vel   = v # Meter :/ Second
    , _mass  = m # Kilo :@ Gram
    }

pos :: Lens' DynState Position
pos = lens (\ds -> (_pos ds) % Meter)
           (\ds pos' -> ds {_pos = pos' # Meter})

vel :: Lens' DynState Velocity
vel = lens (\ds -> (_vel ds) % Meter :/ Second)
           (\ds vel' -> ds {_vel = vel' # Meter :/ Second})

mass :: Lens' DynState Mass
mass = lens (\ds -> (_mass ds) % Kilo :@ Gram)
            (\ds mass' -> ds {_mass = mass' # Kilo :@ Gram})


-- | Delta of the lunar module dynamical state.
data DDynState
  = DDynState
    { _dpos   :: V2         -- m (delta) or m/s (gradient)
    , _dvel   :: V2         -- m/s (delta) or m/s^2 (gradient)
    , _dmass  :: Double     -- kg (delta) or kg/s (gradient)
    } deriving (Show, Generic, AdditiveGroup, VectorSpace)

mkDynStateGradient
  :: Velocity
  -> Acceleration
  -> MassFlowRate
  -> DDynState
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


newtype TGo = TGo { unTGo :: Time } deriving Show

newtype GravAccel = GravAccel { unGravAccel :: Acceleration } deriving Show

data AGCState
  = AGCState
    { _stage :: AscentStage
    , _tgo   :: TGo
    , _gPrev :: GravAccel
    } deriving (Show)
makeLenses ''AGCState

mkAGCState :: AscentStage -> TGo -> GravAccel -> AGCState
mkAGCState = AGCState


-- | Lunar module simulation state.
data LunarModuleSim
  = LunarModuleSim
    { _time      :: Time
    , _agcState  :: AGCState
    , _dynState  :: DynState
    , _dDynState :: DDynState
    } deriving (Show)
makeLenses ''LunarModuleSim

instance AffineSpace DynState where
  type Diff DynState = DDynState
  s1 .-. s2 = DDynState
              { _dpos   = _pos s1 ^-^ _pos s2
              , _dvel   = _vel s1 ^-^ _vel s2
              , _dmass  = _mass s1 ^-^ _mass s2
              }
  s .+^ ds = DynState
             { _pos   = _pos s ^+^ _dpos ds
             , _vel   = _vel s ^+^ _dvel ds
             , _mass  = _mass s ^+^ _dmass ds
             }

mkLunarModuleSim :: Time -> AGCState -> DynState -> DDynState -> LunarModuleSim
mkLunarModuleSim = LunarModuleSim
