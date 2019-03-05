{-|
Module      : Tridiagonal
Description : Define and solve tridiagonal linear systems.

Fast solving of tridiagonal linear systems using a solver that executes in
the 'ST' monad.
-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Tridiagonal
  ( -- * Types
    TriDiagMatrix(..)
    -- * Optics
  , as, bs, cs
    -- * Functions
  , triDiagSolve
  ) where

import           Control.Lens                      ((^.), Lens', lens)
import           Control.Monad.Except              (runExceptT, throwError,
                                                    ExceptT, when)
import           Control.Monad.ST                  (ST)
import qualified Control.Monad.ST                  as ST
import           Control.Monad.Trans.Class         (lift)
import           GHC.TypeNats                      (type (-), KnownNat, Nat)
import qualified Data.STRef                        as STRef
import qualified Data.Vector.Generic               as DVG
import qualified Data.Vector.Generic.Mutable       as DVGM
import qualified Data.Vector.Generic.Sized         as DVGS
import qualified Data.Vector.Unboxed               as DVU
import           Linear.Epsilon                    (Epsilon, nearZero)


-- | Tridiagonal (square) matrix of size @n x n@ elements.
--
--   The layout of elements looks like this, using a @5 x 5@ matrix as an
--   example:
-- 
-- @
-- [ b0 c0        0 ]
-- [ a0 b1 c1       ]
-- [    a1 b2 c2    ]
-- [       a2 b3 c3 ]
-- [  0       a3 b4 ] @
--
data TriDiagMatrix v (n :: Nat) a
  = TriDiagMatrix
    { -- | @as@ are the elements 1 below the diagonal.
      _as :: DVGS.Vector v (n-1) a
      -- | @bs@ are the diagonal elements.
    , _bs :: DVGS.Vector v    n  a
      -- | @cs@ are the elements 1 above the diagonal.
    , _cs :: DVGS.Vector v (n-1) a
    }

-- | Lens for the @as@ field (1 below the diagonal) of a 'TriDiagMatrix'.
as :: forall v n a. Lens' (TriDiagMatrix v n a) (DVGS.Vector v (n-1) a)
as = lens _as (\m _as' -> m {_as = _as'})

-- | Lens for the @bs@ field (diagonal) of a 'TriDiagMatrix'.
bs :: forall v n a. Lens' (TriDiagMatrix v n a) (DVGS.Vector v n a)
bs = lens _bs (\m _bs' -> m {_bs = _bs'})

-- | Lens for the @cs@ field (1 above the diagonal) of a 'TriDiagMatrix'.
cs :: forall v n a. Lens' (TriDiagMatrix v n a) (DVGS.Vector v (n-1) a)
cs = lens _cs (\m _cs' -> m {_cs = _cs'})
  

-- | Solve a 'TriDiagMatrix' linear system in O(N).
--
--   Solves @M * U = R@ for @U@ given @M@ and @R@.
--
--   The solver can fail if any pivot element is near zero (according to the
--   test made available by the 'Epsilon' instance). In these cases, the
--   function returns @Nothing@. Such a failure will definitely occur when the
--   'TriDiagMatrix' is singular, but can also occur in some cases where the
--   matrix is non-singular. It is only guaranteed to succeed when the matrix
--   is diagonally dominant or symmetric positive definite.
--
--   The algorithm used here is the O(N) solver (Tridiagonal matrix algorithm /
--   Thomas algorithm) described in:
--
--     * Press et al. (2007) Numerical Recipes. The Art of Scientific Computing.
--         Cambridge University Press.
--         (Solution of Linear Systems -> Tridiagonal and Band-Diagonal Systems
--          of Equations)
--
--     * Tridiagonal matrix algorithm, Wikipedia:
--         https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm
--
--   The solver itself executes in the 'ST' monad. It allocates two mutable
--   vectors of the same length as the diagonal (one of which becomes the
--   result vector @U@), and an accumulator of type @a@.
triDiagSolve
  :: forall v n a.
     ( DVG.Vector v a
     , KnownNat n
     , Fractional a, Epsilon a )
  => TriDiagMatrix v n a        -- ^ Tridiagonal matrix, @M@.
  -> DVGS.Vector v n a          -- ^ Known right-hand side, @R@.
  -> Maybe (DVGS.Vector v n a)  -- ^ Solution @U@.
triDiagSolve matrix r =
  let
    -- internally, we dispatch to a version working on unsized vectors
    av = DVGS.fromSized (matrix^.as)
    bv = DVGS.fromSized (matrix^.bs)
    cv = DVGS.fromSized (matrix^.cs)
    rv = DVGS.fromSized r
    maybeUV = triDiagSolve' av bv cv rv
  in
    case maybeUV of
      Nothing -> Nothing
      Just uv ->
        -- we make 100% sure it's the correct size at the end
        case (DVGS.toSized uv) of
          Just uvSized -> Just (uvSized)
          Nothing -> error $ "triDiagSolve' returned an invalid vector size! "
                           ++ "This is an internal algorithm error."


-- | Un-sized tridiagonal solver.
triDiagSolve'
  :: forall v a.
     ( DVG.Vector v a
     , Fractional a, Epsilon a )
  => v a          -- ^ a
  -> v a          -- ^ b
  -> v a          -- ^ c
  -> v a          -- ^ r
  -> Maybe (v a)  -- ^ u
triDiagSolve' av bv cv rv =
  let
    -- shortcut for indexing
    (!) :: DVG.Vector v a => v a -> Int -> a
    (!) = (DVG.!)

    -- length of the diagonal
    n :: Int
    n = DVG.length bv

    -- solver in the ST monad
    action :: forall s. ExceptT () (ST s) (v a)
    action = do

      -- check that we don't encounter a zero pivot at the start;
      --  this occurs if bv!0 is zero
      when (nearZero (bv!0)) (throwError ())

      -- result vector
      uv :: DVG.Mutable v s a <- lift . DVGM.new $ n
      -- gamma vector
      gamma :: DVG.Mutable v s a <- lift . DVGM.new $ n

      -- initialize accumulator beta and u vector
      beta <- lift $ STRef.newSTRef $ bv!0
      lift $ DVGM.unsafeWrite uv 0 $ (rv!0) / (bv!0)

      -- decomposition and forward substitution
      cfor 1 (< n) (+ 1) $ \j -> do

        beta_j    <- lift $ STRef.readSTRef beta
        u_jminus1 <- lift $ DVGM.unsafeRead uv (j - 1)

        let
          gamma_j = (cv!(j-1)) / beta_j
          beta_j' = (bv!j - av!(j-1)) * gamma_j

        -- check for a zero pivot
        when (nearZero beta_j') (throwError ())

        let u_j = (rv!j - (av!(j-1)) * u_jminus1) / beta_j'

        lift $ DVGM.unsafeWrite gamma j gamma_j
        lift $ DVGM.unsafeWrite uv j u_j
        lift $ STRef.writeSTRef beta beta_j'

      -- backsubstitution
      cfor (n-2) (>= 0) (\x -> x - 1) $ \j -> do

        u_j          <- lift $ DVGM.unsafeRead uv j
        u_jplus1     <- lift $ DVGM.unsafeRead uv (j+1)
        gamma_jplus1 <- lift $ DVGM.unsafeRead gamma (j+1)

        let u_j' = u_j - gamma_jplus1*u_jplus1

        lift $ DVGM.unsafeWrite uv j u_j'
        
      -- freeze and return the vector
      lift . DVG.unsafeFreeze $ uv

    either2Maybe :: Either () b -> Maybe b
    either2Maybe (Left ()) = Nothing
    either2Maybe (Right x) = Just x
  
  in either2Maybe $ ST.runST $ runExceptT $ action

{-# SPECIALIZE triDiagSolve'
    :: DVU.Vector Double
    -> DVU.Vector Double
    -> DVU.Vector Double
    -> DVU.Vector Double
    -> Maybe (DVU.Vector Double) #-}
{-# SPECIALIZE triDiagSolve'
    :: DVU.Vector Float
    -> DVU.Vector Float
    -> DVU.Vector Float
    -> DVU.Vector Float
    -> Maybe (DVU.Vector Float) #-}


-- | C-style for loop.
cfor
  :: forall a m.
     ( Monad m )
  => a            -- ^ initial value
  -> (a -> Bool)  -- ^ continue?
  -> (a -> a)     -- ^ step
  -> (a -> m ())  -- ^ body of the loop
  -> m ()         -- ^ looping action
cfor initVal cont step body = go initVal
  where
    go :: a -> m ()
    go x
      | cont x    = body x >> go (step x)
      | otherwise = pure()
{-# INLINE cfor #-}
