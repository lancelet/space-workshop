{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module ODE.FixedStepV where

import           Data.AffineSpace   (AffineSpace, Diff, (.+^))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.VectorSpace   (Scalar, VectorSpace, (*^))


-- | Integration with a given list of steps.
integrate
  :: forall as vs a.
     ( AffineSpace as
     , vs ~ Diff as, VectorSpace vs
     , a ~ Scalar vs, Num a )
  => Stepper a as vs   -- ^ Step function to use.
  -> as                -- ^ Initial state.
  -> NonEmpty a        -- ^ List of steps.
  -> ((a, as) -> vs)   -- ^ Function: (variable, state) -> gradient.
  -> NonEmpty (a, as)  -- ^ Output state steps.
integrate stepper y0 xs f =
  let
    x0 :: a
    x0 = NonEmpty.head xs
    
    stepFn :: (a, as) -> a -> (a, as)
    stepFn (xi, yi) xf = stepper (xf - xi) f (xi, yi)
  in
    NonEmpty.scanl stepFn (x0, y0) xs


-- | Stepper function.
type Stepper a as vs
  = a               -- ^ Step size.
 -> ((a, as) -> vs) -- ^ Gradient function.
 -> (a, as)         -- ^ Integration value and state before step.
 -> (a, as)         -- ^ Final state.


-- | Single step of Euler integration.
--
--   TODO: Make attendees write this.
eulerStep
  :: ( AffineSpace as
     , vs ~ Diff as, VectorSpace vs
     , a ~ Scalar vs, Num a )
  => a                -- ^ Step size.
  -> ((a, as) -> vs)  -- ^ Gradient function.
  -> (a, as)          -- ^ Integration value and state before step.
  -> (a, as)          -- ^ Final state.
eulerStep h f q@(x, y) = (x + h, y .+^ h *^ f q)


-- | Single step of 4th order Runge-Kutta integration.
--
--   TODO: Make attendees write this.
rk4Step
  :: forall as vs a.
     ( AffineSpace as
     , vs ~ Diff as, VectorSpace vs
     , a ~ Scalar vs, Fractional a )
  => a                -- ^ Step size.
  -> ((a, as) -> vs)  -- ^ Gradient function.
  -> (a, as)          -- ^ Integration value and state before step.
  -> (a, as)          -- ^ Final state.
rk4Step h f (x, y) =
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
