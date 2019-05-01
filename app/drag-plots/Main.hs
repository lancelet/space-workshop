{-|
Module      : Main.hs
Description : Plotting of drag curves.
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Drag
import qualified Plot

main :: IO ()
main = putStrLn "TODO"

plotKephartDragCurves :: FilePath -> IO ()
plotKephartDragCurves _ =
  let
    x1, x2, x3, x4 :: [(Double, Double)]
    ms :: [Double]
    ms = [ 0.00, 0.01 .. 9.0 ]

    x1 = [ (m, Drag.unDragCoeff (Drag.kephartDragSplined Drag.LowDrag (Drag.Mach m)))
         | m <- ms ]
    x2 = [ (m, Drag.unDragCoeff (Drag.kephartDragSplined Drag.SolidRocket (Drag.Mach m)))
         | m <- ms ]
    x3 = [ (m, Drag.unDragCoeff (Drag.kephartDragSplined Drag.LiquidRocket (Drag.Mach m)))
         | m <- ms ]
    x4 = [ (m, Drag.unDragCoeff (Drag.kephartDragSplined Drag.HighDrag (Drag.Mach m)))
         | m <- ms ]
  in
    Plot.xyChart
    Plot.Screen
    "Drag Curves for Various Rocket Types"
    "M - Mach Number"
    "Cd - Drag Coefficient"
    [ Plot.Line "Low-drag configuration" x1
    , Plot.Line "Solid rocket" x2
    , Plot.Line "Big liquid rocket" x3
    , Plot.Line "High-drag configuration" x4 ]
