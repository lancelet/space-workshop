{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Main where

import qualified Graphics.Gnuplot.Simple as Gnuplot
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import qualified Data.Maybe as Maybe
import qualified Data.Vector.Unboxed.Sized as USized

import qualified ODE

main :: IO ()
main = do
  shm

shm :: IO ()
shm = do
  let
    state0 = Maybe.fromJust $ USized.fromList [ 1.0, 0.0 ]
    integResult = ODE.odeFixedSteps ODE.eulerStep [0.0, 0.1 .. 5.0] state0 shmfn
    plotValues = fmap (\(t, v) -> (t, USized.index v 0)) integResult
  putStrLn "Hello World"
  Gnuplot.plotList [Gnuplot.terminal (SVG.cons "test.svg")] plotValues

shmfn :: (Double, USized.Vector 2 Double) -> USized.Vector 2 Double
shmfn (_, s) = Maybe.fromJust $ USized.fromList [ v, -k * x / m ]
  where
    m = 1.0
    k = 1.0
    x = USized.index s 0
    v = USized.index s 1


