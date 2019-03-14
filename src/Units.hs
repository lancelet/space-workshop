module Units
  ( Length
  , Mass
  , Time
  , Velocity
  , Acceleration
  ) where

import qualified Data.Dimensions.SI     as D
import           Data.Metrology.Show    ()
import qualified Data.Metrology.SI.Poly as SIPoly
import qualified Data.Metrology.Vector  as DMV

-- Make a quantity that uses the SI LCSU and a custom numeric type.
type MkQu_DLSI dim = DMV.Qu (DMV.DimFactorsOf dim) SIPoly.SI

type Length       = MkQu_DLSI D.Length
type Mass         = MkQu_DLSI D.Mass
type Time         = MkQu_DLSI D.Time

type Velocity     = MkQu_DLSI D.Velocity
type Acceleration = MkQu_DLSI D.Acceleration
