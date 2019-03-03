{-|
Module      : Main
Description : Validation plots for the McDrag code
Maintainer  : Jonathan Merritt <j.s.merritt@gmail.com>
-}
module Main where

import Control.Monad (forM_)
import qualified Graphics.Gnuplot.Advanced             as GP
import qualified Graphics.Gnuplot.Frame                as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet      as Opts
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification    as LineSpec
import qualified Graphics.Gnuplot.Plot.TwoDimensional  as Plot2D
import qualified Graphics.Gnuplot.Terminal.SVG         as SVG
import Text.Printf (printf)

import qualified McDrag

main :: IO ()
main = do

  putStrLn "McDrag Validation"

  -- Using 5.56mm, BRL-1 projectile
  let
    params :: McDrag.McParams Float
    params = McDrag.McParams
      { McDrag.d_REF = 5.7 / 1000.0 -- m
      , McDrag.l_T = 5.48 -- calibers
      , McDrag.l_N = 3.0 -- calibers
      , McDrag.hsp = 0.5 -- head shape parameter
      , McDrag.l_BT = 1.0 -- calibers
      , McDrag.d_B = 0.754 -- calibers
      , McDrag.d_M = 0.0 -- no meplat
      , McDrag.d_RB = 1.0 -- no rotating band
      , McDrag.bl = McDrag.LaminarOnNose
      }
    cs = McDrag.SpeedOfSound 343.0
    nu = McDrag.KinVisc 1.46e-5

    f :: Float -> McDrag.McBasicOut Float
    f m = McDrag.mcDragBasic params cs nu (McDrag.Mach m)

    ms :: [Float]
    ms = [ 0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 0.925, 0.95, 0.975, 1
         , 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 2, 2.2, 2.5, 3, 3.5, 4 ]

    xs :: [(Float, McDrag.McBasicOut Float)]
    xs = [ (m, f m) | m <- [0.0, 0.01 .. 5.0] ]

  let
    ec :: (McDrag.McBasicOut Float -> b)
       -> [(Float, McDrag.McBasicOut Float)]
       -> [(Float, b)]
    ec fn = fmap (\(m, r) -> (m, fn r))

    aboveZero :: [(Float, Float)] -> [(Float, Float)]
    aboveZero = fmap (\(m, r) -> (m, max 0 r))

    c_DHS = ec McDrag.c_DHS xs
    c_DHT = aboveZero $ ec McDrag.c_DHT xs
    c_DBT = aboveZero $ ec McDrag.c_DBT xs
    c_DSF = ec McDrag.c_DSF xs
    c_DB = fmap (\(m,r) -> (m, min 1 r)) $ ec McDrag.c_DB xs
    -- c_PBI = aboveZero $ fmap (\(m,r) -> (m, min 1 r)) $ ec McDrag.c_PBI xs
    -- c_PBIA = aboveZero $ fmap (\(m,r) -> (m, min 1 r)) $ ec McDrag.c_PBIA xs
  
         
    oplt :: [(Float, McDrag.McBasicOut Float)]
    oplt = [ (m, f m) | m <- ms ]

  
  _ <- GP.plot
    (SVG.cons "test.svg")
    (Frame.cons
      ( Opts.title "Components of drag"
      $ Opts.xLabel "M"
      $ Opts.yLabel "C_d"
      $ Opts.gridXTicks True
      $ Opts.gridYTicks True
      $ Opts.deflt
      )
    (   (setTitle "Head (supersonic)"     $ Plot2D.list Graph2D.lines c_DHS)
     <> (setTitle "Head (transonic)"      $ Plot2D.list Graph2D.lines c_DHT)
     <> (setTitle "Boattail"              $ Plot2D.list Graph2D.lines c_DBT)
     <> (setTitle "Skin friction"         $ Plot2D.list Graph2D.lines c_DSF)
     <> (setTitle "Blunt base"            $ Plot2D.list Graph2D.lines c_DB)
     -- <> (setTitle "Pressure ratio"        $ Plot2D.list Graph2D.lines c_PBI)
     -- <> (setTitle "Pressure ratio (alt)"  $ Plot2D.list Graph2D.lines c_PBIA)
    ))

  --        -----   -----   -----   -----   -----
  putStrLn "    M    CDSF    CDBT     CDB   PB/PI"
  let
    fmt :: Float -> String
    fmt f = printf "%.3f" (if isNaN f then 0.0 else f)
  forM_ oplt $ \(m, r) ->
    putStrLn
      $  fmt m <> "   "
      <> fmt (McDrag.c_DSF r) <> "   "
      <> fmt (max 0 (McDrag.c_DBT r)) <> "   "
      <> fmt (McDrag.c_DB r) <> "   "
      <> fmt (McDrag.c_PBI r)
  
  pure ()
    

setTitle :: Functor f => String -> f (Graph2D.T x y) -> f (Graph2D.T x y)
setTitle title = fmap (Graph2D.lineSpec (LineSpec.title title $ LineSpec.deflt))


  {-
  let
    f :: Float -> Float
    f x = McDrag.unHeadDrag $
          McDrag.headDrag
          (McDrag.Mach x)
          (McDrag.HeadLength 3.03)
          (McDrag.HeadShape 0.5)
          (McDrag.HeadMeplatDiam 0.09)

    g :: Float -> Float
    g x = McDrag.unBoatTailDrag $
          McDrag.boatTailDrag
          (McDrag.Mach x)
          (McDrag.HeadLength 3.03)
          (McDrag.HeadShape 0.5)
          (McDrag.HeadMeplatDiam 0.09)
          (McDrag.BodyCylinderLength 2.3)
          (McDrag.BoatTailAngle (7.5 * pi / 180.0))
          (McDrag.BoatTailLength 0.579)
    
    xs = [ (x, g x) | x <- [0.0, 0.001 .. 5.0] ]


  let
    plot :: Plot2D.T Float Float
    plot = fmap (Graph2D.lineSpec (LineSpec.title "C_{dbt}" $ LineSpec.deflt)) $ Plot2D.list Graph2D.lines xs
  _ <- GP.plot (SVG.cons "test.svg") $ Frame.cons
    ( Opts.title "Components of drag vs Mach number"
      $ Opts.xLabel "M"
      $ Opts.yLabel "C_d"
      $ Opts.gridXTicks True
      $ Opts.gridYTicks True
      $ Opts.deflt
    ) plot


  putStrLn "Hello World"
  -}
