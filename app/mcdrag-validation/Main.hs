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
import qualified Graphics.Gnuplot.Terminal.PostScript  as PS
import Text.Printf (printf)

import qualified McDrag

main :: IO ()
main = do

  putStrLn "McDrag Validation"

  let
    params :: McDrag.McParams Float
    -- 5.56mm, BRL-1 projectile
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
    -- minuteman
    {- 
    params = McDrag.McParams
      { McDrag.d_REF = 55.6 / 1000.0 -- m
      , McDrag.l_T = 3.250 -- calibers
      , McDrag.l_N = 0.967 -- calibers
      , McDrag.hsp = 0 -- head shape parameter
      , McDrag.l_BT = 1.180 -- calibers
      , McDrag.d_B = 1.630 -- calibers
      , McDrag.d_M = 0.200 -- no meplat
      , McDrag.d_RB = 1.00 -- no rotating band
      , McDrag.bl = McDrag.FullyTurbulent
      }
    -}
    -- cs = McDrag.SpeedOfSound 343.0
    -- nu = McDrag.KinVisc 1.46e-5

    f :: Float -> McDrag.McBasicOut Float
    f m = McDrag.mcDragBasic params (McDrag.Mach m)

    ms :: [Float]
    ms = [ 0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 0.925, 0.95, 0.975, 1
         , 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 2, 2.2, 2.5, 3, 3.5, 4 ]

    xs :: [(Float, McDrag.McBasicOut Float)]
    xs = [ (m, f m) | m <- [0.0, 0.01 .. 9.0] ]

  let
    ec :: (McDrag.McBasicOut Float -> b)
       -> [(Float, McDrag.McBasicOut Float)]
       -> [(Float, b)]
    ec fn = fmap (\(m, r) -> (m, fn r))

    c_D = ec McDrag.c_D xs
    c_DH = ec McDrag.c_DH xs
    c_DBT = ec McDrag.c_DBT xs
    c_DSF = ec McDrag.c_DSF xs
    c_DB = ec McDrag.c_DB xs
    -- c_PBI = aboveZero $ fmap (\(m,r) -> (m, min 1 r)) $ ec McDrag.c_PBI xs
    -- c_PBIA = aboveZero $ fmap (\(m,r) -> (m, min 1 r)) $ ec McDrag.c_PBIA xs
  
         
    oplt :: [(Float, McDrag.McBasicOut Float)]
    oplt = [ (m, f m) | m <- ms ]

  
  _ <- GP.plot
    (SVG.cons "test.svg")
    (Frame.cons
      ( Opts.title "MC DRAG : Components of drag vs Mach number"
      $ Opts.xLabel "M"
      $ Opts.yLabel "C_d"
      $ Opts.gridXTicks True
      $ Opts.gridYTicks True
      $ Opts.deflt
      )
    (   (setTitle "Total"                 $ Plot2D.list Graph2D.lines c_D)
     <> (setTitle "Head"                  $ Plot2D.list Graph2D.lines c_DH)
     <> (setTitle "Boattail"              $ Plot2D.list Graph2D.lines c_DBT)
     <> (setTitle "Skin friction"         $ Plot2D.list Graph2D.lines c_DSF)
     <> (setTitle "Blunt base"            $ Plot2D.list Graph2D.lines c_DB)
    ))

  --        -----   -----   -----   -----   -----   -----   -----
  putStrLn "    M     CD0     CDH    CDSF    CDBT     CDB   PB/PI"
  let
    fmt :: Float -> String
    fmt = printf "%.3f"
  forM_ oplt $ \(m, r) ->
    putStrLn
      $  fmt m <> "   "
      <> fmt (McDrag.c_D r) <> "   "
      <> fmt (McDrag.c_DH r) <> "   "
      <> fmt (McDrag.c_DSF r) <> "   "
      <> fmt (max 0 (McDrag.c_DBT r)) <> "   "
      <> fmt (McDrag.c_DB r) <> "   "
      <> fmt (McDrag.c_PBI r)
  
  pure ()
    

setTitle :: Functor f => String -> f (Graph2D.T x y) -> f (Graph2D.T x y)
setTitle title = fmap (Graph2D.lineSpec (LineSpec.title title $ LineSpec.deflt))
