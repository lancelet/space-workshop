module Main where

import qualified Test.DocTest as DocTest
import qualified Test.Tasty as Tasty

import qualified TestTridiagonal as Tridiagonal


main :: IO ()
main = doctests
    >> Tasty.defaultMain tests 


tests :: Tasty.TestTree
tests = Tasty.testGroup "Tests"
  [ Tridiagonal.tests
  ]


doctests :: IO ()
doctests = do
  putStrLn "\nRunning doctests ..." 
  DocTest.doctest [ "-isrc", "src/ODE.hs" ]
  putStrLn "... done running doctests.\n"
