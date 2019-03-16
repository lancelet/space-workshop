{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Orphans where


import           Data.AdditiveGroup     (AdditiveGroup, negateV, zeroV, (^+^),
                                         (^-^))
import           Data.Basis             (Basis, HasBasis, basisValue, decompose,
                                         decompose')
import qualified Data.Dimensions.SI     as D
import qualified Data.Metrology.SI.Poly as SIPoly
import           Data.Metrology.Vector  (( # ), (%))
import qualified Data.Metrology.Vector  as DMV
import           Data.Units.SI.Parser   (si)
import           Data.VectorSpace       (InnerSpace, Scalar, VectorSpace,
                                         magnitude, (*^), (<.>))
import qualified Linear                 as L


instance (Num a) => AdditiveGroup (L.V2 a) where
  zeroV = L.V2 0 0
  v1 ^+^ v2 = v1 L.^+^ v2
  v1 ^-^ v2 = v1 L.^-^ v2
  negateV v = L.negated v


instance (Num a) => VectorSpace (L.V2 a) where
  type Scalar (L.V2 a) = a
  s *^ v = s L.*^ v


instance (Num a, AdditiveGroup a) => InnerSpace (L.V2 a) where
  v1 <.> v2 = L.dot v1 v2


-- | Provide a HasBasis instance for time units.
--
-- This seems needlessly messy; I expect there's a better way!
instance
  forall n s d l.
  ( VectorSpace n, InnerSpace n, HasBasis n, () ~ Basis n
  , s ~ Scalar n, Floating s
  , d ~ DMV.DimFactorsOf D.Time
  , l ~ SIPoly.SI
  ) => HasBasis (DMV.Qu d l n) where
  type Basis (DMV.Qu d l n) = ()
  basisValue () =
    let
      bvn :: n
      bvn = basisValue ()
    in
      bvn % [si| s |]
  decompose x = [ ((), magnitude(x # [si| s |])) ]
  decompose' x _ = magnitude(x # [si| s |])
