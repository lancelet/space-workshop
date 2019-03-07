{-|
Module      : Drag
Description : Drag approximation.
-}
module Drag
  ( -- * Types
    Mach(..)
  , DragCoeff(..)
  , KephartType(..)
    -- * Functions
  , kephartDrag
  ) where

-- | Mach number.
newtype Mach a = Mach a

-- | Drag coefficient.
newtype DragCoeff a = DragCoeff a

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

kephartDrag
  :: (Ord a, Floating a)
  => Mach a
  -> KephartType
  -> DragCoeff a
kephartDrag (Mach m) kt =
  let

    c1 = case kt of
           LowDrag      -> 0.05
           SolidRocket  -> 0.1
           LiquidRocket -> 0.2
           HighDrag     -> 0.35

    c2 = case kt of
           LowDrag -> 4.7
           _       -> 5.3

    a = c1 + 0.0007 * exp c2

    b | m < 6     = 0.0267*m*m - 0.0012*m*m*m - 0.193*m + 0.557
      | otherwise = 0.1

    c = case kt of
          LowDrag      -> 0.45*b
          SolidRocket  -> b
          LiquidRocket -> b*(1.357 - 0.039*m)
          HighDrag     -> b*(1.85 - 0.0756*m)
           
    d = case kt of
          LowDrag      -> 0.045
          SolidRocket  -> 0.11
          LiquidRocket -> 0.14
          HighDrag     -> b
  
    cd | m <= 1.1  = a
       | m <= 6    = c
       | otherwise = d

  in DragCoeff cd
