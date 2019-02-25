{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module ClearCoat.Escher
  ( -- * Types
    Tri(Tri)
  , TriMesh(TriMesh)
  , Path(Path)
  , PathWidth(PathWidth)
  , MitreLimit(MitreLimit)
    -- * Functions
  , strokePathMitre
  ) where

import           Control.Lens              ((^.))
import qualified Data.Foldable             as Foldable
import qualified Data.Vector.Storable      as V
import           Foreign.Storable          (Storable, alignment, peek,
                                            peekElemOff, poke, sizeOf)
import qualified Graphics.Rendering.OpenGL as GL
import           Linear                    (V2 (V2), norm, (*^), (^+^), (^-^),
                                            _x, _y)
import qualified Linear                    as Linear

import           ClearCoat.Types           (MitreLimit (MitreLimit),
                                            Path (Path), PathWidth (PathWidth))

data Tri
  = Tri {-# UNPACK #-} !(GL.Vertex2 Float)
        {-# UNPACK #-} !(GL.Vertex2 Float)
        {-# UNPACK #-} !(GL.Vertex2 Float)
  deriving (Eq, Show)

newtype TriMesh
  = TriMesh [Tri]

instance Semigroup TriMesh where
  (TriMesh x) <> (TriMesh y) = TriMesh (x <> y)

gv :: V2 Float -> GL.Vertex2 Float
gv (V2 x y) = GL.Vertex2 x y

-- | Stroke a path with a mitre limit.
strokePathMitre
  :: MitreLimit -- ^ Mitre limit.
  -> PathWidth  -- ^ Width of the path.
  -> Path       -- ^ Incoming path (must have at least 2 points).
  -> TriMesh    -- ^ Triangular mesh.
strokePathMitre (MitreLimit m) (PathWidth w) (Path vsf) =
  let
    strokeSegmentPairs :: [V2 Float] -> TriMesh
    strokeSegmentPairs (v1 : v2 : v3 : remainder)
      = segmentRectangle v1 v2
     <> segmentMitre v1 v2 v3
     <> strokeSegmentPairs (v2 : v3 : remainder)
    strokeSegmentPairs [v1, v2]
      = segmentRectangle v1 v2
    strokeSegmentPairs _
      = error "strokePath called on a single vertex - not a path!"

    w2 :: Float
    w2 = w / 2.0

    segmentRectangle :: V2 Float -> V2 Float -> TriMesh
    segmentRectangle v1 v2 =
      let
        np = w2 *^ (Linear.perp . Linear.normalize $ v2 ^-^ v1)
        q1 = v1 ^+^ np
        q2 = v1 ^-^ np
        q3 = v2 ^-^ np
        q4 = v2 ^+^ np
      in
        TriMesh
        [ Tri (gv q1) (gv q2) (gv q3)
        , Tri (gv q1) (gv q3) (gv q4)
        ]

    segmentMitre :: V2 Float -> V2 Float -> V2 Float -> TriMesh
    segmentMitre v1 v2 v3 =
      let
        n12 = Linear.normalize $ v2 ^-^ v1
        n23 = Linear.normalize $ v3 ^-^ v2
        alpha = atan2 (n12^._y) (n12^._x)
        beta = atan2 (n23^._y) (n23^._x)
        alpha' = if alpha >= 0 then alpha else (2 * pi + alpha)
        beta' = if beta >= 0 then beta else (2 * pi + beta)
        theta = alpha' - beta'
        np12 = w2 *^ Linear.perp n12
        np23 = w2 *^ Linear.perp n23
        ns12 = if theta < 0 then np12 else (-np12)
        ns23 = if theta < 0 then np23 else (-np23)

        q1 = v2
        q2 = v2 ^+^ ns23
        q4 = v2 ^+^ ns12

        -- solve for the intersection point q3
        q3 = intersectLines (q4, n12) (q2, n23)
        t = norm (q3 ^-^ q4)
      in
        -- handle the mitre limit case
        if t <= m
        then
          TriMesh
          [ Tri (gv q1) (gv q2) (gv q3)
          , Tri (gv q3) (gv q4) (gv q1)
          ]
        else
          let
            q3a = q4 ^+^ (m *^ n12)
            q3b = q2 ^-^ (m *^ n23)
          in
            TriMesh
            [ Tri (gv q1) (gv q3a) (gv q4)
            , Tri (gv q1) (gv q3b) (gv q3a)
            , Tri (gv q1) (gv q2) (gv q3b)
            ]
  in
    strokeSegmentPairs (Foldable.toList vsf)


intersectLines
  :: (V2 Float, V2 Float) -- ^ Point and normal 1
  -> (V2 Float, V2 Float) -- ^ Point and normal 2
  -> V2 Float             -- ^ Intersection
intersectLines (p, v) (q, n) =
  let
    s = (p^._y - q^._y) + (v^._y / v^._x) * (q^._x - p^._x) /
        (n^._y - (v^._y * n^._x / v^._y))
  in
    q ^+^ (s *^ n)
