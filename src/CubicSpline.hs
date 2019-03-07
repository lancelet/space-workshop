{-|
Module      : CubicSpline
Description : Cubic spline interpolation.
-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CubicSpline
  ( -- * Functions
    interpolate
  , interpolate'
  , knots
  ) where

import           Control.Monad.ST              (runST)
import           Data.Ord                      (comparing)
import qualified Data.Vector.Algorithms.Intro  as Intro
import qualified Data.Vector.Algorithms.Search as Search
import qualified Data.Vector.Generic           as DVG
import           Linear.Epsilon                (Epsilon)

import           Tridiagonal                   (TriDiagMatrix, triDiagMatrix',
                                                triDiagSolve)


-- | Cubic spline interpolation of a function.
interpolate
  :: forall v a.
     ( DVG.Vector v (a, a), DVG.Vector v a
     , Ord a, Fractional a, Epsilon a )
  => v (a, a)
  -> Maybe (a -> a)
interpolate xys =
  let xyss = sortedX xys
  in case knots xyss of
    Nothing -> Nothing
    Just ks ->
      let
        xys' = unpack xyss  -- xys but sorted
        xMin = fst . DVG.head $ xys'
        xMax = fst . DVG.last $ xys'
        yMin = snd . DVG.head $ xys'
        yMax = snd . DVG.last $ xys'

        f x
          | x <= xMin = yMin + (xMin - x) * DVG.head ks
          | x >= xMax = yMax + (x - xMax) * DVG.last ks
          | otherwise =
            let
              i = (binarySearchX xyss x) - 1
              (xn, yn) = xys' DVG.! i
              (xp, yp) = xys' DVG.! (i+1)
              kn = ks DVG.! i
              kp = ks DVG.! (i+1)
              dx = xp - xn
              dy = yp - yn
              a = (x - xn) / dx  -- t
              b = 1 - a
              c = ( kn)*dx - dy
              d = (-kp)*dx + dy

            in b*yn + a*yp + a*b*(c*b + d*a)
      in Just f


interpolate'
  :: forall v a.
     ( DVG.Vector v (a, a), DVG.Vector v a
     , Ord a, Fractional a, Epsilon a )
  => v (a, a)
  -> (a -> a)
interpolate' xys
  = case interpolate xys of
      Nothing -> error "Cannot interpolate less than 2 points!"
      Just f  -> f


-- | A vector @v@ containing @(x,y)@ tuples that are sorted according to
--   increasing @x@ values.
newtype SortedX v a = SortedX { unpack :: v (a, a) }


-- | Sort a vector according to its x coordinate.
sortedX
  :: forall v a.
     ( DVG.Vector v (a, a)
     , Ord a )
  => v (a, a)     -- ^ Input vector.
  -> SortedX v a  -- ^ Type with sorted x values.
sortedX xys = runST $ do
  mv <- DVG.thaw xys
  Intro.sortBy (comparing fst) mv
  SortedX <$> DVG.unsafeFreeze mv


binarySearchX
  :: ( DVG.Vector v (a, a)
     , Ord a )
  => SortedX v a
  -> a
  -> Int
binarySearchX (SortedX v) value = runST $ do
  mv <- DVG.unsafeThaw v
  Search.binarySearchBy (comparing fst) mv (value, undefined)


-- | Determine the knot vector of a cubic spline.
--
--   The knot vector consists of the approximated first derivative of the
--   interpolating function.
--
--   This function will fail if the vector supplied has a length less than
--   two (ie. at least two points are required for interpolation. The input
--   vector must also be sorted according to increasing x values otherwise
--   the results will be incorrect.
knots
  :: forall v a.
     ( DVG.Vector v (a, a), DVG.Vector v a
     , Fractional a, Epsilon a )
  -- | Vector of (x, y) coordinates to be interpolated. This vector must be
  --   sorted according to increasing x values in order for the result to be
  --   valid.
  => SortedX v a
  -- | Resulting knot vector.
  -> Maybe (v a)
knots (SortedX xys) =
  let
    xs, ys :: v a
    xs = DVG.map fst xys
    ys = DVG.map snd xys

    dxs, dys :: v a
    dxs = DVG.zipWith (-) (DVG.tail xs) xs
    dys = DVG.zipWith (-) (DVG.tail ys) ys

    x0, xf, y0, yf :: a
    x0 = DVG.head dxs
    xf = DVG.last dxs
    y0 = DVG.head dys
    yf = DVG.last dys

    ac :: v a
    ac = DVG.map (\x -> 1/x) dxs

    b, bCentral :: v a
    bCentral = DVG.zipWith (\xi xj -> 2*(1/xi + 1/xj)) dxs (DVG.tail dxs)
    b = DVG.fromList $ (  [2/x0]
                       ++ (DVG.toList bCentral)
                       ++ [2/xf] )

    matrix :: TriDiagMatrix v a
    matrix = triDiagMatrix' ac b ac

    r, rCentral :: v a
    rCentral = DVG.zipWith4 (\xi xj yi yj -> 3*(yi/xi/xi + yj/xj/xj))
               dxs (DVG.tail dxs) dys (DVG.tail dys)
    r = DVG.fromList $ (  [3*y0/x0/x0]
                       ++ (DVG.toList rCentral)
                       ++ [3*yf/xf/xf] )

    k = case triDiagSolve matrix r of
          Nothing     -> error "Cubic spline solution failed: singular matrix!"
          Just kValue -> kValue

    kMinLength | DVG.length xys < 2 = Nothing
               | otherwise          = Just k
  in kMinLength
