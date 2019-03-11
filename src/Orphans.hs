{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Orphans where


import           Data.AdditiveGroup (AdditiveGroup, negateV, zeroV, (^+^),
                                     (^-^))
import           Data.VectorSpace   (Scalar, VectorSpace, (*^))
import qualified Linear             as L


instance (Num a) => AdditiveGroup (L.V2 a) where
  zeroV = L.V2 0 0
  v1 ^+^ v2 = v1 L.^+^ v2
  v1 ^-^ v2 = v1 L.^-^ v2
  negateV v = L.negated v


instance (Num a) => VectorSpace (L.V2 a) where
  type Scalar (L.V2 a) = a
  s *^ v = s L.*^ v
