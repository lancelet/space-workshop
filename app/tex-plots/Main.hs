{-|
Module      : Main.hs
Description : Plotting diagrams for the notes.
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad        (forM_)
import           System.IO            (hFlush, stdout)

import qualified Examples.ODEExamples as ODEExamples
import qualified Hohmann              as Hohmann
import qualified LunarAscent          as LunarAscent
import qualified Plot                 as Plot
import qualified Staging              as Staging


main :: IO ()
main = do
  let plots
        = [ (ODEExamples.plotEulerDoubleExpDecay, "euler-double-exp-decay")
          , (ODEExamples.plotEulerSHM, "euler-shm")
          , (ODEExamples.plotSHMComparison, "shm-comparison")
          , (LunarAscent.plotLunarAscentVerticalRise, "lunar-ascent-vertical-rise")
          , (LunarAscent.plotLunarAscentBurnOnly, "lunar-ascent-burn-only")
          , (LunarAscent.plotLunarAscentMoonView, "lunar-ascent-moon-view")
          , (Staging.plotVelocityComparison, "staging-velocity-comparison")
          , (ODEExamples.plotVerticalThrow, "vertical-throw")
          , (Hohmann.plotHighImpulseBurn, "hohmann-high-impulse")
          , (Hohmann.plotLowImpulseBurn, "hohmann-low-impulse")
          , (Hohmann.plotUltraLowImpulseBurn, "hohmann-ultra-low-impulse")
          ]

  putStrLn "Plotting diagrams for the notes."
  forM_ plots $ \(plotFn, name) -> plotPGF plotFn name
  putStrLn "Done plotting diagrams for the notes."


plotPGF :: (Plot.Output -> IO ()) -> FilePath -> IO ()
plotPGF plotFn name = do
  let
    fullName = "./notes/fig/" ++ name ++ ".tex"

    fgReset = "\x1b[0m"
    fgGreen = "\x1b[32m"
    fgBlue  = "\x1b[34m"

    inBlue x = fgBlue ++ x ++ fgReset
    inGreen x = fgGreen ++ x ++ fgReset

  putStr $ "  -> Plotting " ++ (inBlue fullName) ++ "..."
  hFlush stdout
  plotFn (Plot.PGF fullName)
  putStr $ " " ++ (inGreen "DONE!") ++ "\n"
  hFlush stdout
