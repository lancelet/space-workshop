module Main where

import qualified Test.Tasty as Tasty

import qualified TestTridiagonal as Tridiagonal


main :: IO ()
main = Tasty.defaultMain tests


tests :: Tasty.TestTree
tests = Tasty.testGroup "Tests"
  [ Tridiagonal.tests
  ]
