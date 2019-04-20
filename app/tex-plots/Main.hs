{-|
Module      : Main.hs
Description : Plotting diagrams for the notes.
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Examples.ODEExamples as ODEExamples
import qualified Plot as Plot

main :: IO ()
main = do
  putStrLn "Plotting diagrams for the notes."
  ODEExamples.plotEulerDoubleExpDecay (Plot.PGF "./notes/fig/euler-double-exp-decay.tex")
  putStrLn "Done plotting diagrams for the notes."


  
