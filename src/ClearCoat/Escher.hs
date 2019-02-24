{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module ClearCoat.Escher where

import           Control.Lens  ((^.))
import qualified Data.Foldable as Foldable
import           Linear        (V2, (*^), (^+^), (^-^), _x, _y)
import qualified Linear        as Linear

data Tri a
  = Tri {-# UNPACK #-} !(V2 a)
        {-# UNPACK #-} !(V2 a)
        {-# UNPACK #-} !(V2 a)
  deriving (Eq, Show)

newtype TriMesh t a
  = TriMesh (t (Tri a))

instance (forall q. Semigroup (t q)) => Semigroup (TriMesh t a) where
  (TriMesh x) <> (TriMesh y) = TriMesh (x <> y)

newtype Path t a
  = Path (t (V2 a))

newtype PathWidth a = PathWidth a

newtype MitreLimit a = MitreLimit a


strokePathMitre
  :: forall t a.
     ( Foldable t
     , RealFloat a, Linear.Epsilon a )
  => MitreLimit a
  -> PathWidth a
  -> Path t a
  -> TriMesh [] a
strokePathMitre (MitreLimit m) (PathWidth w) (Path vsf) =
  let
    strokeSegmentPairs :: [V2 a] -> TriMesh [] a
    strokeSegmentPairs (v1 : v2 : v3 : remainder)
      = segmentRectangle v1 v2
     <> segmentMitre v1 v2 v3
     <> strokeSegmentPairs (v2 : v3 : remainder)
    strokeSegmentPairs [v1, v2]
      = segmentRectangle v1 v2
    strokeSegmentPairs _
      = error "strokePath called on a single vertex - not a path!"

    w2 :: a
    w2 = w / 2.0

    segmentRectangle :: V2 a -> V2 a -> TriMesh [] a
    segmentRectangle v1 v2 =
      let
        np = w2 *^ (Linear.perp . Linear.normalize $ v2 ^-^ v1)
        q1 = v1 ^+^ np
        q2 = v1 ^-^ np
        q3 = v2 ^-^ np
        q4 = v2 ^+^ np
      in
        TriMesh
        [ Tri q1 q2 q3
        , Tri q1 q3 q4
        ]

    segmentMitre :: V2 a -> V2 a -> V2 a -> TriMesh [] a
    segmentMitre v1 v2 v3 =
      let
        n12 = Linear.normalize $ v2 ^-^ v1
        n23 = Linear.normalize $ v3 ^-^ v2
        alpha = atan2 (n12^._y) (n12^._x)
        beta = atan2 (n23^._y) (n23^._x)
        theta = alpha - beta
        np12 = w2 *^ Linear.perp n12
        np23 = w2 *^ Linear.perp n23
        ns12 = if theta > 0 then np12 else (-np12)
        ns23 = if theta > 0 then np23 else (-np23)

        q1 = v2
        q2 = v2 ^+^ ns23
        q4 = v2 ^+^ ns12

        -- solve for the intersection point q3
        t = q4^._y + (n12^._y / n12^._x) * (q2^._x - q2^._x) - q2^._y /
            ( n12^._y * n23^._x / n12^._x - n23^._y)
        q3 = q4 ^+^ (t *^ n12)
      in
        -- handle the mitering case
        if t <= m
        then
          TriMesh
          [ Tri q1 q2 q3
          , Tri q3 q4 q1
          ]
        else
          let
            q3a = q4 ^+^ (m *^ n12)
            q3b = q2 ^-^ (m *^ n23)
          in
            TriMesh
            [ Tri q1 q3a q4
            , Tri q1 q3b q3a
            , Tri q1 q2 q3b
            ]
  in
    strokeSegmentPairs (Foldable.toList vsf)

