{-|
Module      : Main.hs
Description : Plotting of drag curves.
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

main :: IO ()
main = putStrLn "TODO"

{-

import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Graphics.Gnuplot.Advanced             as GP
import qualified Graphics.Gnuplot.Frame                as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet      as Opts
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification    as LineSpec
import qualified Graphics.Gnuplot.Plot.TwoDimensional  as Plot2D
import qualified Graphics.Gnuplot.Terminal.PostScript  as PS
import qualified Graphics.Gnuplot.Terminal.SVG         as SVG
import           System.Exit                           (ExitCode)

import qualified Drag


main :: IO ()
main = do
  _ <- plotKephartDragCurves "kephart-drag-curves.svg"
  pure ()


plotKephartDragCurves :: Text -> IO ExitCode
plotKephartDragCurves fileName =
  let
    x1, x2, x3, x4 :: [(Float, Float)]
    ms :: [Float]
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
    GP.plot
    (SVG.cons (T.unpack fileName))
    (Frame.cons
      ( Opts.title "Drag Curves for Various Rocket Types - Reproduced from Kephert (1971)"
      $ Opts.xLabel "M - Mach Number"
      $ Opts.yLabel "C_d - Drag Coefficient"
      $ Opts.gridXTicks True
      $ Opts.gridYTicks True
      $ Opts.deflt
      )
      (  plotPairs  "\"Low-drag\" configuration (X=1)" x1
      <> plotPairs                "Solid rocket (X=2)" x2
      <> plotPairs           "Big liquid rocket (X=3)" x3
      <> plotPairs "\"High-drag\" configuration (X=4)" x4
      ))


plotPairs :: Text -> [(Float, Float)] -> Plot2D.T Float Float
plotPairs title values = setTitle title (Plot2D.list Graph2D.lines values)


setTitle :: Functor f => Text -> f (Graph2D.T x y) -> f (Graph2D.T x y)
setTitle title = fmap (Graph2D.lineSpec
                       ( LineSpec.title (T.unpack title) $ LineSpec.deflt )
                      )
-}
