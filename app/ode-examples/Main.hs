{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Main where

import qualified ODE.Examples as ODEExamples

main :: IO ()
main = do
  ODEExamples.plotSHM
