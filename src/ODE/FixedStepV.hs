{-# LANGUAGE TypeFamilies #-}
module ODE.FixedStepV where

import Data.AffineSpace (AffineSpace, Diff, (.+^))
import Data.VectorSpace (VectorSpace, Scalar, (*^))

eulerStep
  :: ( AffineSpace s
     , d ~ Diff s, VectorSpace d
     , a ~ Scalar d, Num a )
  => a              -- ^ Step size.
  -> (a, s)         -- ^ Integration value and state before step.
  -> ((a, s) -> d)  -- ^ Gradient function.
  -> (a, s)         -- ^ Final state.
eulerStep h q@(x, y) f = (x + h, y .+^ h *^ f q)
