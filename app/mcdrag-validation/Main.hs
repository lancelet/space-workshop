{-|
Module      : Main
Description : Validation plots for the McDrag code
Maintainer  : Jonathan Merritt <j.s.merritt@gmail.com>
-}
module Main where

import qualified Graphics.Gnuplot.Simple       as Gnuplot
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
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
    xs = [ (x, f x) | x <- [0.0, 0.001 .. 4.5] ]
  
  Gnuplot.plotLists
    [ Gnuplot.Key Nothing
    , Gnuplot.XLabel "Mach number"
    , Gnuplot.YLabel "C_d"
    , Gnuplot.terminal (SVG.cons "test.svg")
    ]
    [xs]
  
  putStrLn "Hello World"
