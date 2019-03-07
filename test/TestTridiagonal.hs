{-|
Module      : TestTridiagonal
Description : Tests of the Tridiagonal module.
-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TestTridiagonal
  ( tests
  ) where

import           Data.Ratio          (Ratio, (%))
import qualified Data.Vector         as DV
import           Hedgehog            ((===))
import qualified Hedgehog            as H
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Linear              (Epsilon, nearZero)
import qualified Test.Tasty          as Tasty
import           Test.Tasty.Hedgehog (testProperty)

import           Tridiagonal         (TriDiagMatrix, as, bs, cs, triDiagMatrix',
                                      triDiagSolve)

tests :: Tasty.TestTree
tests = Tasty.testGroup "Tridiagonal"
  [ testProperty "triDiagSolve solves a range of diagonally dominant matrices"
                 prop_triDiagSolve_diagDominant
  , testProperty "triDiagSolve solves an example problem"
                 prop_triDiagSolve_example
  , testProperty "triDiagSolve refuses to solve a singular matrix"
                 prop_triDiagSolve_singular
  ]


-- | Orphan Annie for 'Ratio' / 'Rational Integer'.
instance (Integral a) => Epsilon (Ratio a) where
  nearZero x = (x == 0)


-- | Property that 'triDiagSolve' can solve a diagonally-dominant matrix.
prop_triDiagSolve_diagDominant :: H.Property
prop_triDiagSolve_diagDominant = H.property $ do
  n          <- H.forAll $ Gen.integral (Range.linear 1 100)
  matrix     <- H.forAll $ genDiagDominantRational n

  let genElem =
        let range = Range.linearFrom 0 (-1000) (1000)
        in fromIntegral <$> Gen.int16 range
  u_expected <- H.forAll $ DV.replicateM n genElem

  let r = mulTriDiagV matrix u_expected
  H.annotate ("r = " <> show r)
  let u_actual = triDiagSolve matrix r

  u_actual === Just u_expected


-- | Test 'triDiagSolve' on a single known example. This is mostly a
--   sanity-check, as an example where the matrices are not automatically
--   generated.
prop_triDiagSolve_example :: H.Property
prop_triDiagSolve_example = H.withTests 1 $ H.property $
  let
    matrix :: TriDiagMatrix DV.Vector Rational
    matrix = triDiagMatrix' (DV.fromList [4, 2, 5, 9, 4])
                            (DV.fromList [3, 1, 6, 8, 3, 4])
                            (DV.fromList [1, 5, 5, 9, 2])

    r = DV.fromList [1, 2, 3, 4, 5, 6]

    u_expected = DV.fromList
                 [ 64%379, 187%379, 63%379, 77%379, 65%379, 1007%758 ]

  in triDiagSolve matrix r === Just u_expected


-- | Test that 'triDiagSolve' returns 'Nothing' when trying to solve a
--   system with a singular matrix.
prop_triDiagSolve_singular :: H.Property
prop_triDiagSolve_singular = H.withTests 1 $ H.property $
  let
    matrix :: TriDiagMatrix DV.Vector Rational
    matrix = triDiagMatrix' (DV.fromList [ 2%3 ])
                            (DV.fromList [ (-1), (-1) ])
                            (DV.fromList [ 3%2 ])
    r = DV.fromList [1, 2]

  in triDiagSolve matrix r === Nothing


-- | Multiply a 'TriDiagMatrix' by a vector.
mulTriDiagV
  :: forall a.
     ( Num a )
  => TriDiagMatrix DV.Vector a
  -> DV.Vector a
  -> DV.Vector a
mulTriDiagV matrix u =
  let
    adot = DV.cons 0 (DV.zipWith (*) (as matrix) (DV.init u))
    bdot = DV.zipWith (*) (bs matrix) u
    cdot = DV.snoc (DV.zipWith (*) (cs matrix) (DV.tail u)) 0
  in
    DV.zipWith3 (\au bu cu -> au + bu + cu) adot bdot cdot


-- | Generate a diagonally-dominant 'TriDiagMatrix' containing 'Rational'
--   numbers.
genDiagDominantRational
  :: forall m. ( H.MonadGen m ) => Int -> m (TriDiagMatrix DV.Vector Rational)
genDiagDominantRational size = do

  let
    -- all diagonal entries are in +/-[diagMin, diagMax]
    -- all non-diagonal entries are in (approx) (-diagMin/2, diagMin/2)
    diagMin = 500
    diagMax = 1000
    odMax = floor (((fromIntegral diagMin) :: Double) / 2.0) - 1

    genSign = Gen.bool >>= \b -> pure (if b then 1 else (-1))

    bGen = fromIntegral <$>
             ((*) <$> Gen.int16 (Range.linear diagMin diagMax)
                  <*> genSign)
    acGen = fromIntegral <$>
            Gen.int16 (Range.linear (-odMax) (odMax))

  triDiagMatrix' <$> DV.replicateM (size-1) acGen
                 <*> DV.replicateM size     bGen
                 <*> DV.replicateM (size-1) acGen
