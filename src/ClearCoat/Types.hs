module ClearCoat.Types where

import Linear (V2)

newtype Path
  = Path [V2 Float]

newtype PathWidth = PathWidth Float

newtype MitreLimit = MitreLimit Float
