{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DataKinds #-}
module Units
  ( -- * Base units
    Count
  , Length
  , Mass
  , Time
    -- * Standard derived units
  , Velocity
  , Acceleration
  , AngVelocity
  , AngAcceleration
  , Force
    -- * Apollo-specific derived units
  , MassFlowRate
  , SGPUnit
  , BThresholdUnit
    -- * Metric units
  , Second(Second)
    -- * Imperial units
  , Foot(Foot)
  , Pound(Pound)
    -- * Quasiquoter
  , si
    -- * Unit operators
  , (:/)((:/)), (%), (%.), (|*^|), (|*|), (|+|), (|-|), (|.+^|), (|.-.|)
  , (|.|), (|/), (|/|), (|^*|), (|^/|), type(@+), (|^), (*|), (:^)((:^)), (#)
  , (.#)
    -- * Unit functions
  , qDistanceSq, qMagnitudeSq, qNegate, qSq, qNormalized, quantity, zero
  , qMagnitude
    -- * Additional unit types
  , Normalize, Point(Point), Qu, Three
  ) where

import qualified Data.Dimensions.SI     as D
import           Data.Metrology.Show    ()
import qualified Data.Metrology.SI.Poly as SIPoly
import           Data.Metrology.Vector  ((:/) ((:/)), (:^) ((:^)), type (@+),
                                         Normalize, Point (Point), Qu, Three,
                                         Two, MOne, MTwo, qDistanceSq, qMagnitude,
                                         qMagnitudeSq, qNegate, qNormalized,
                                         qSq, quantity, zero, ( # ), (%), (%.),
                                         (*|), (|*^|), (|*|), (|+|), (|-|),
                                         (|.+^|), (|.-.|), (|.|), (|/), (|/|),
                                         (|^), (|^*|), (|^/|), (.#))
import qualified Data.Metrology.Vector  as DMV
import           Data.Units.SI          (Second (Second))
import           Data.Units.SI.Parser   (si)
import           Data.Units.US          (Foot (Foot), Pound (Pound))


-- Make a quantity that uses the SI LCSU and a custom numeric type.
type MkQu_DLSI dim = DMV.Qu (DMV.DimFactorsOf dim) SIPoly.SI

type Count           = Qu '[] SIPoly.SI

type Length          = MkQu_DLSI D.Length
type Mass            = MkQu_DLSI D.Mass
type Time            = MkQu_DLSI D.Time

type Velocity        = MkQu_DLSI D.Velocity
type Acceleration    = MkQu_DLSI D.Acceleration
type AngVelocity     = MkQu_DLSI (D.Time :^ MOne)
type AngAcceleration = MkQu_DLSI (D.Time :^ MTwo)
type Force           = MkQu_DLSI D.Force

type MassFlowRate    = MkQu_DLSI (D.Mass :/ D.Time)
type SGPUnit         = MkQu_DLSI (D.Length :^ Three :/ D.Time :^ Two)
type BThresholdUnit  = MkQu_DLSI (D.Length :/ D.Time :^ Three)
