{-|
Module      : Main
Description : Validation plots for the McDrag code
Maintainer  : Jonathan Merritt <j.s.merritt@gmail.com>
-}
module Main where

import qualified Graphics.Gnuplot.Advanced             as GP
import qualified Graphics.Gnuplot.Frame                as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet      as Opts
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification    as LineSpec
import qualified Graphics.Gnuplot.Plot.TwoDimensional  as Plot2D
import qualified Graphics.Gnuplot.Terminal.SVG         as SVG

import qualified McDrag

main :: IO ()
main = do

  let
    f :: Float -> Float
    f x = McDrag.unHeadDrag $
          McDrag.headDrag
          (McDrag.Mach x)
          (McDrag.HeadLength 3.03)
          (McDrag.HeadShape 0.5)
          (McDrag.HeadMeplatDiam 0.09)
    xs = [ (x, f x) | x <- [0.0, 0.001 .. 5.0] ]


  let
    plot :: Plot2D.T Float Float
    plot = fmap (Graph2D.lineSpec (LineSpec.title "C_{dh}" $ LineSpec.deflt)) $ Plot2D.list Graph2D.lines xs
  _ <- GP.plot (SVG.cons "test.svg") $ Frame.cons
    ( Opts.title "Components of drag vs Mach number"
      $ Opts.xLabel "M"
      $ Opts.yLabel "C_d"
      $ Opts.gridXTicks True
      $ Opts.gridYTicks True
      $ Opts.deflt
    ) plot


  putStrLn "Hello World"
