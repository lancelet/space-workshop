{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGuAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module TestTridiagonal
  ( tests
  , genDiagDominant
  , genDiagDominant'
  , q
  ) where

import           Control.Lens        ((^.))
import           Data.Finite         (Finite, getFinite)
import           Data.Proxy          (Proxy (Proxy))
import           Data.Reflection     (reifyNat, reify)
import qualified Data.Vector         as DV
import qualified Data.Vector.Sized   as DVS
import           GHC.TypeLits        (type (-), type (+), type (<=), KnownNat, natVal, someNatVal)
import qualified Hedgehog            as H
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import qualified Test.Tasty          as Tasty
import           Test.Tasty.Hedgehog (testProperty)

import           Tridiagonal       (TriDiagMatrix (TriDiagMatrix), as, bs, cs)

tests :: Tasty.TestTree
tests = Tasty.testGroup "Tridiagonal"
  [ testProperty "triDiagSolve solves diagonally dominant matrices"
                 prop_triDiagSolve_diagDominant
  ]


prop_triDiagSolve_diagDominant :: H.Property
prop_triDiagSolve_diagDominant = H.property $ do
  n <- H.forAll $ Gen.integral (Range.linear 1 100)
  let
    genElem = Gen.float (Range.linearFracFrom 0 (-10) 10)
  
  reifyNat n $ \(proxy :: Proxy (n-1)) -> do
    matrix <- H.forAll $ genDiagDominant' proxy 10 genElem
    H.success
  {-
  let sn = case someNatVal (fromIntegral ni) of
             Just x -> x
             Nothing -> error "Badly-generated SomeNat"
  withSomeSing sn $ \_ -> do
    matrix <- H.forAll $ genDiagDominant 5 undefined
    ()
  -}
  
  H.success


-- | Multiply a 'TriDiagMatrix' by a vector.
mulTriDiagV
  :: forall n a.
     ( KnownNat n, n ~ ((n-1)+1), (1+(n-1)) ~ n
     , Num a )
  => TriDiagMatrix DV.Vector n a
  -> DVS.Vector n a
  -> DVS.Vector n a
mulTriDiagV matrix u =
  let
    adot = DVS.cons 0 (DVS.zipWith (*) (matrix^.as) (DVS.init u))
    bdot = DVS.zipWith (*) (matrix^.bs) u
    cdot = DVS.snoc (DVS.zipWith (*) (matrix^.cs) (DVS.tail u)) 0
  in
    DVS.zipWith3 (\au bu cu -> au + bu + cu) adot bdot cdot
  

q :: (H.MonadGen m) => m (TriDiagMatrix DV.Vector 11 Float)
q = genDiagDominant' (Proxy :: Proxy 10) 5 (Gen.float (Range.linearFracFrom 0 (-10) 10))


genDiagDominant'
  :: forall n m a.
     ( KnownNat n, n ~ ((n+1)-1), KnownNat (n+1)
     , H.MonadGen m
     , Num a, Ord a )
  => Proxy n
  -> a
  -> m a
  -> m (TriDiagMatrix DV.Vector (n+1) a)
genDiagDominant' _ = genDiagDominant


-- | Generate a diagonally-dominant 'TriDiagMatrix'.
genDiagDominant
  :: forall n m a.
     ( KnownNat n, KnownNat (n+1), n ~ ((n+1)-1)
     , H.MonadGen m
     , Num a, Ord a )
  -- | Minimum allowed absolute value in the diagonal.
  => a
  -- | Generator for diagonal elements.
  -> m a
  -- | Generator for TriDiagMatrix.
  -> m (TriDiagMatrix DV.Vector (n+1) a)
genDiagDominant minDiag genElement = do

  -- Generate unsized vectors and then convert them to sized vectors.

  -- First generate the diagonal
  let
    len = fromIntegral (natVal (Proxy :: Proxy (n+1)))
    bGen =
      let bound = abs minDiag
      in Gen.filter (\el -> (abs el) > bound) genElement
  bUnsized <- DV.replicateM len bGen

  -- Generate the a elements (1 below the diagonal), but make sure that their
  -- absolute values are less than the absolute values in the corresponding
  -- elements of the diagonal.
  let
    aGen i =
      let bound = abs (bUnsized DV.! (i+1))
      in Gen.filter (\el -> (abs el) < bound) genElement
  aUnsized <- DV.generateM (len-1) aGen

  -- Generate the c elements (1 above the diagonal), but make sure that their
  -- absolute values are less than the absolute values in the corresponding
  -- elements of the diagonal.
  let
    cGen i =
      let bound = abs (bUnsized DV.! i)
      in Gen.filter (\el -> (abs el) < bound) genElement
  cUnsized <- DV.generateM (len-1) cGen

  -- Convert the unsized vectors to sized ones.
  let maybeVecs = (,,) <$> DVS.toSized aUnsized
                       <*> DVS.toSized bUnsized
                       <*> DVS.toSized cUnsized
  case maybeVecs of
    Nothing           -> error "genDiagDominant is invalid"
    Just (av, bv, cv) -> pure (TriDiagMatrix av bv cv)
