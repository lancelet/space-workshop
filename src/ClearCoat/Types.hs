module ClearCoat.Types where

import Linear (V2)

newtype Path t a
  = Path (t (V2 a))

newtype PathWidth a = PathWidth a

newtype MitreLimit a = MitreLimit a
