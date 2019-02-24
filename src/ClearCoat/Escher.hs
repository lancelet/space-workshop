{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuantifiedConstraints #-}

module ClearCoat.Escher where

import Linear (V2(V2))

data Tri a
  = Tri
    { tri1 :: {-# UNPACK #-} !(V2 a)
    , tri2 :: {-# UNPACK #-} !(V2 a)
    , tri3 :: {-# UNPACK #-} !(V2 a)
    } deriving (Eq, Show)

newtype TriMesh t a
  = TriMesh (t (Tri a))

instance (forall q. Semigroup (t q)) => Semigroup (TriMesh t a) where
  (TriMesh x) <> (TriMesh y) = TriMesh (x <> y)

  
{-
instance (b ~ Tri a, Semigroup (t b)) => Semigroup (TriMesh t a) where
  (TriMesh x) <> (TriMesh y) = TriMesh (x <> y)
-}

newtype Path f a
  = Path (f (V2 a))

newtype PathWidth a = PathWidth a

newtype MitreLimit a = MitreLimit a


strokePath
  ::
     MitreLimit a
  -> PathWidth a
  -> Path f a
  -> TriMesh f a
strokePath = undefined

{-
stroke2PathSegment
  ::
-}
     
