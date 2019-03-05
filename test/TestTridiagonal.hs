{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module TestTridiagonal
  ( tests
  ) where

import           Data.Finite       (Finite, getFinite)
import           Data.Proxy        (Proxy (Proxy))
import qualified Data.Vector       as DV
import qualified Data.Vector.Sized as DVS
import           GHC.TypeNats      (type (-), KnownNat, natVal)
import qualified Hedgehog          as H
import qualified Hedgehog.Gen      as Gen
import qualified Hedgehog.Range    as Range
import qualified Test.Tasty        as Tasty

import           Tridiagonal       (TriDiagMatrix (TriDiagMatrix))

tests :: Tasty.TestTree
tests = Tasty.testGroup "Tridiagonal"
  [
  ]


-- | Generate a diagonally-dominant matrix.
genDiagDominant
  :: forall n m a.
     ( KnownNat n, KnownNat (n-1)
     , H.MonadGen m
     , Num a, Ord a )
  -- | Minimum allowed absolute value in the diagonal.
  => a
  -- | Generator for diagonal elements.
  -> m a
  -- | Generator for TriDiagMatrix.
  -> m (TriDiagMatrix DV.Vector n a)  -- ^ Generator for TriDiagMatrix.
genDiagDominant minDiag genElement = do

  -- Generate unsized vectors and then convert them to sized vectors.

  -- First generate the diagonal
  let
    len = fromIntegral (natVal (Proxy :: Proxy n))
    bGen =
      let bound = abs minDiag
      in Gen.filter (\el -> abs el > bound) genElement
  bUnsized <- DV.replicateM len bGen

  -- Generate the a elements (1 below the diagonal), but make sure that their
  -- absolute values are greater than the absolute values in the
  -- corresponding elements of the diagonal.
  let
    aGen i =
      let bound = abs (bUnsized DV.! (i-1))
      in Gen.filter (\el -> abs el < bound) genElement
  aUnsized <- DV.generateM (len-1) aGen

  -- Generate the c elements (1 above the diagonal), but make sure that their
  -- absolute values are less than the absolute values in the corresponding
  -- elements of the diagonal.
  let
    cGen i =
      let bound = abs (bUnsized DV.! i)
      in Gen.filter (\el -> abs el < bound) genElement
  cUnsized <- DV.generateM (len-1) cGen

  -- Convert the unsized vectors to sized ones.
  let maybeVecs = (,,) <$> DVS.toSized aUnsized
                       <*> DVS.toSized bUnsized
                       <*> DVS.toSized cUnsized
  case maybeVecs of
    Nothing -> error "genDiagDominant is invalid"
    Just (av, bv, cv) -> pure (TriDiagMatrix av bv cv)
