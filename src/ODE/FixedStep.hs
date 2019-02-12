{-# LANGUAGE ScopedTypeVariables #-}

module ODE.FixedStep where

import Data.List.NonEmpty (NonEmpty, (<|))
import qualified Data.List.NonEmpty as NonEmpty 

-- | Operations required for a type that can be integrated.
--
--   @v@ is a vector / container type
--   @a@ is a scalar type, usually corresponding to elements
--       contained within v
data Ops v a
  = Ops
    { scalarMul :: a -> v -> v
    , addVec    :: v -> v -> v }


-- | Integration operations for a 2-tuple.
tuple2Ops :: (Num a) => Ops (a,a) a
tuple2Ops
  = Ops
    { scalarMul = \c (x, y) -> (c * x, c * y)
    , addVec = \(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)
    }


-- | Stepper function.
type Stepper v a
  = Ops v a
 -> a
 -> (a, v)
 -> (a -> v -> v)
 -> (a, v)


-- | Performs integration given a nonempty list of steps.
integrate
  :: forall a v. (Num a, Fractional a)
  => Ops v a           -- ^ Operations on vector type v.
  -> Stepper v a       -- ^ Stepper function.
  -> v                 -- ^ Initial state.
  -> NonEmpty a        -- ^ Nonempty list of steps.
  -> (a -> v -> v)     -- ^ Variable and state to derivatives.
  -> NonEmpty (a, v)   -- ^ Output of the integration.
integrate ops stepper y0 xs f
  = (x0, y0) <| NonEmpty.scanl step (x0, y0) xs
  where
    x0 :: a
    x0 = NonEmpty.head xs

    step :: (a, v) -> a -> (a, v)
    step (ti, yi) tf = stepper ops (tf - ti) (ti, yi) f

  
-- | Performs a single step of Euler integration.
--
--   TODO: Make attendees write this (elsewhere).
eulerStep
  :: forall a v. (Num a)
  => Ops v a        -- ^ Operations on vector type v.
  -> a              -- ^ Step size.
  -> (a, v)         -- ^ Variable and state before step.
  -> (a -> v -> v)  -- ^ Variable and state to derivatives.
  -> (a, v)         -- ^ Variable and state after step.
eulerStep ops h (x, y) f = (x + h, y ^+^ h *^ f x y)
  where
    (^+^) = addVec ops
    (*^) = scalarMul ops
    infixl 6 ^+^
    infixl 7 *^


-- | Performs a single step of Runge-Kutta integration.
--
--   TODO: Make attendees write this (elsewhere).
rk4Step
  :: forall a v. (Num a, Fractional a)
  => Ops v a
  -> a
  -> (a, v)
  -> (a -> v -> v)
  -> (a, v)
rk4Step ops h (x, y) f =
  let
    (^+^) = addVec ops
    (*^) = scalarMul ops
    infixl 6 ^+^
    infixl 7 *^

    k1 = h *^ f x y
    k2 = h *^ f (x + 0.5 * h) (y ^+^ 0.5 *^ k1)
    k3 = h *^ f (x + 0.5 * h) (y ^+^ 0.5 *^ k2)
    k4 = h *^ f (x + h) (y ^+^ k3)

  in
      (x + h, y ^+^ (1.0/6.0) *^ k1
                ^+^ (1.0/3.0) *^ k2
                ^+^ (1.0/3.0) *^ k3
                ^+^ (1.0/6.0) *^ k4)
