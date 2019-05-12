{-|
Module      : Hohmann.Types
Description : Types for the Hohmann module.

These types are separated so that they can be imported by both the Hohmann and
Solutions.Hohmann modules, without introducing cyclic dependencies.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
module Hohmann.Types where

import           Data.AdditiveGroup (AdditiveGroup)
import           Data.AffineSpace   (AffineSpace, Diff, (.+^), (.-.))
import           Data.VectorSpace   (VectorSpace, (^+^), (^-^))
import           GHC.Generics       (Generic)
import           Linear             (V2)
import           Orphans            ()


-- | Fixed parameters for the Hohmann simulation.
data Params
  = Params
    { mu    :: Double  -- ^ Standard gravitational parameter (m^3/s^2).
    , mDot  :: Double  -- ^ Mass flow rate (kg/s)
    , isp   :: Double  -- ^ Vacuum specific impulse (s).
    , r1    :: Double  -- ^ Inner circular orbit radius (m).
    , r2    :: Double  -- ^ Outer circular orbit radius (m).
    , tStep :: Double  -- ^ Time step (s).
    , tEps  :: Double  -- ^ t_epsilon: accuracy of start/stop (s).
    }


-- | State of the system.
data State
  = State
    { mass     :: Double
    , distance :: Double
    , position :: V2 Double
    , velocity :: V2 Double
    }
  deriving (Show)


-- | Delta in state of the system.
data DState
  = DState
    { dMass     :: Double
    , dDistance :: Double
    , dPosition :: V2 Double
    , dVelocity :: V2 Double
    }
  deriving (Show, Generic, AdditiveGroup, VectorSpace)


-- | State is an AffineSpace instance, with DState as its associated vector
--   space.
instance AffineSpace State where
  type Diff State = DState
  s1 .-. s2 = DState
              { dMass     = mass s1 - mass s2
              , dDistance = distance s2 ^-^ distance s2
              , dPosition = position s1 ^-^ position s2
              , dVelocity = velocity s1 ^-^ velocity s2
              }
  s .+^ ds = State
             { mass     = mass s + dMass ds
             , distance = distance s + dDistance ds
             , position = position s ^+^ dPosition ds
             , velocity = velocity s ^+^ dVelocity ds
             }
