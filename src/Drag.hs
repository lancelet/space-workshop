{-|
Module      : Drag
Description : Drag approximation.


The drag coefficient model in this module is from:

  * Kephart, DC (1971) Boost: On-line computer program for estimating
      powered-rocket performance.
      United States Air Force Project Rand. R-670-PR.

The module implements a raw model that matches the program listing from the
original paper, but also a cubic-spline-interpolated model that allows for
interpolation between the various rocket "types", and slightly smooths-over
the discontinuous peaks of the original drag curves.
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Drag
  ( -- * Types
    Mach(..)
  , DragCoeff(..)
  , KephartType(..)
    -- * Functions
  , kephartDrag
  , kephartDragSplined
  ) where

import qualified Data.Vector    as DV
import           Linear.Epsilon (Epsilon)

import qualified CubicSpline    (interpolate')


-- | Mach number.
newtype Mach a = Mach a


-- | Drag coefficient.
newtype DragCoeff a = DragCoeff { unDragCoeff :: a }


-- | Type of rocket based on the original designation of drag curves from
--   Kephart (1971).
data KephartType
  = -- | "Low-drag" configuration.
    LowDrag       -- X = 1
    -- | Solid rocket (Minuteman, Poseidon, Spartan, Sprint).
  | SolidRocket   -- X = 2
    -- | Big liquid rocket.
  | LiquidRocket  -- X = 3
    -- | "High-drag" configuration.
  | HighDrag      -- X = 4


kephartDragSplined
  :: forall a.
     (Ord a, Enum a, Floating a, Epsilon a)
  => KephartType
  -> Mach a
  -> DragCoeff a
kephartDragSplined kt =
  let
    samples :: DV.Vector (a, a)
    samples =
      DV.fromList $
      [ (m, unDragCoeff (kephartDrag kt (Mach m)))
      -- These exact sampling points require a little finessing because of the
      -- discontinuities in the drag.
      | m <- [ 0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.925, 0.95, 0.975, 1.0, 1.025
             , 1.05, 1.075, 1.10, 1.15, 1.2, 1.4, 1.8, 2, 3, 4, 5, 7, 9 ] ]

    interpFn :: a -> a
    interpFn = CubicSpline.interpolate' samples

  in \(Mach m) -> DragCoeff (interpFn m)



-- | Compute drag coefficient very similarly to Kephart (1971).
kephartDrag
  :: (Ord a, Floating a)
  => KephartType   -- ^ Rocket type (Kephart 1971).
  -> Mach a        -- ^ Mach number.
  -> DragCoeff a   -- ^ Drag coefficient.
kephartDrag kt (Mach m) =
  let

    c1 = case kt of
           LowDrag      -> 0.05
           SolidRocket  -> 0.1
           LiquidRocket -> 0.2
           HighDrag     -> 0.35

    c2 = case kt of
           LowDrag -> 4.7
           _       -> 5.3

    a = c1 + 0.0007 * (exp (m*c2))

    b | m < 6     = 0.0267*m*m - 0.0012*m*m*m - 0.193*m + 0.557
      | otherwise = 0.1

    c = case kt of
          LowDrag      -> 0.46*b
          SolidRocket  -> b
          LiquidRocket -> b*(1.357 - 0.039*m)
          HighDrag     -> b*(1.85 - 0.0756*m)

    d = case kt of
          LowDrag      -> 0.045
          SolidRocket  -> b
          LiquidRocket -> 0.11
          HighDrag     -> 0.14

    -- This is different from Kephart: here we do a very small amount of
    -- blending near the apex of the curve.
    acMix =
      let f = smootherstep ((m - 0.98) / (1.10 - 0.98))
      in f*c + (1 - f)*a
  
    cd | m <= 0.98 = a
       | m <= 1.10 = acMix
       | m <= 6    = c
       | otherwise = d

  in DragCoeff cd


smootherstep :: (Ord a, Num a) => a -> a
smootherstep x
  | x <= 0    = 0
  | x <  1    = x*x*x*(x*(x*6 - 15) + 10)
  | otherwise = 1
