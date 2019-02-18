{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module ODE.FixedStepV where

import           Data.AffineSpace (AffineSpace, Diff, (.+^))
import           Data.VectorSpace (Scalar, VectorSpace, (*^))


eulerStep
  :: ( AffineSpace as
     , vs ~ Diff as, VectorSpace vs
     , a ~ Scalar vs, Num a )
  => a                -- ^ Step size.
  -> (a, as)          -- ^ Integration value and state before step.
  -> ((a, as) -> vs)  -- ^ Gradient function.
  -> (a, as)          -- ^ Final state.
eulerStep h q@(x, y) f = (x + h, y .+^ h *^ f q)


rk4Step
  :: forall as vs a.
     ( AffineSpace as
     , vs ~ Diff as, VectorSpace vs
     , a ~ Scalar vs, Fractional a )
  => a                -- ^ Step size.
  -> (a, as)          -- ^ Integration value and state before step.
  -> ((a, as) -> vs)  -- ^ Gradient function.
  -> (a, as)          -- ^ Final state.
rk4Step h (x, y) f =
  let
    sixth, third :: a
    sixth = (1.0 / 6.0)
    third = (1.0 / 3.0)
  in
    let
      k1, k2, k3, k4 :: vs
      k1 = h *^ f (x, y)
      k2 = h *^ f (x + 0.5 * h, y .+^ 0.5 *^ k1)
      k3 = h *^ f (x + 0.5 * h, y .+^ 0.5 *^ k2)
      k4 = h *^ f (x + h, y .+^ k3)
    in
      ( x + h
      , y .+^ sixth *^ k1
          .+^ third *^ k2
          .+^ third *^ k3
          .+^ sixth *^ k4 )
