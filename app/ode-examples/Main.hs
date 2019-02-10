{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Main where

import qualified Graphics.Rendering.Chart.Easy as Chart
import qualified Graphics.Rendering.Chart.Backend.Diagrams as ChartDiagrams
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
    integResult = ODE.odeFixedSteps ODE.eulerStep [0.0, 0.01 .. 15.0] state0 shmfn
    plotValues = fmap (\(t, v) -> (t, USized.index v 0)) integResult
  putStrLn "Hello World"
  ChartDiagrams.toFile Chart.def "test.svg" $ do
    Chart.plot (Chart.line "integrated" [plotValues])

shmfn :: (Double, USized.Vector 2 Double) -> USized.Vector 2 Double
shmfn (_, s) = Maybe.fromJust $ USized.fromList [ v, -k * x / m ]
  where
    m = 1.0
    k = 3.0
    x = USized.index s 0
    v = USized.index s 1


