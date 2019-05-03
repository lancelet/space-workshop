{-|
Module      : Staging.Types
Description : Types for the Staging module.

These types are separated so that they can be imported by both the Staging and
Solutions.Staging modules, without introducing cyclic dependencies.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
module Staging.Types where

import           Data.AdditiveGroup (AdditiveGroup)
import           Data.AffineSpace   (AffineSpace, Diff, (.+^), (.-.))
import           Data.VectorSpace   (VectorSpace)
import           GHC.Generics       (Generic)

-- | State of the system.
data State
  = State
    { propellantMass :: Double
    , position       :: Double
    , velocity       :: Double
    }
  deriving (Show)

-- | Delta in state of the system.
data DState
  = DState
    { dPropellantMass :: Double
    , dPosition       :: Double
    , dVelocity       :: Double
    }
  deriving (Show, Generic, AdditiveGroup, VectorSpace)

-- | State is an AffineSpace instance, with DState as its associated vector
--   space.
instance AffineSpace State where
  type Diff State = DState
  s1 .-. s2 = DState
              { dPropellantMass = propellantMass s1 - propellantMass s2
              , dPosition       = position s1 - position s2
              , dVelocity       = velocity s1 - velocity s2
              }
  s .+^ ds = State
             { propellantMass = propellantMass s + dPropellantMass ds
             , position       = position s + dPosition ds
             , velocity       = velocity s + dVelocity ds
             }
