module ODE where

import qualified Todo

test :: Double -> Double
test x = x * 2

test' :: Double -> Double
test' = Todo.todo (Todo.FallbackSolution test)
