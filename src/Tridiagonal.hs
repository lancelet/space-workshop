{-|
Module      : Tridiagonal
Description : Define and solve tridiagonal linear systems.

Fast solving of tridiagonal linear systems using a solver that executes in
the 'ST' monad.
-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tridiagonal
  ( -- * Types
    TriDiagMatrix(as, bs, cs)
    -- * Functions
  , triDiagMatrix
  , triDiagMatrix'
  , triDiagSolve
  ) where

import           Control.Monad.Except        (ExceptT, runExceptT, throwError,
                                              when)
import           Control.Monad.ST            (ST)
import qualified Control.Monad.ST            as ST
import           Control.Monad.Trans.Class   (lift)
import qualified Data.STRef                  as STRef
import qualified Data.Vector.Generic         as DVG
import qualified Data.Vector.Generic.Mutable as DVGM
import qualified Data.Vector.Unboxed         as DVU
import           Linear.Epsilon              (Epsilon, nearZero)


-- | Tridiagonal (square) matrix.
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
data TriDiagMatrix v a
  = TriDiagMatrix
    { -- | @as@ are the elements 1 below the diagonal. This vector has one less
      --   element than the diagonal.
      as :: v a
      -- | @bs@ are the diagonal elements.
    , bs :: v a
      -- | @cs@ are the elements 1 above the diagonal. This vector has one less
      --   element than the diagonal.
    , cs :: v a
    } deriving (Show)


-- | Create a 'TriDiagMatrix' if all dimensions of the vectors are correct.
triDiagMatrix
  :: ( DVG.Vector v a )
  -- | Vector of @n-1@ elements, below the diagonal.
  => v a
  -- | Vector of @n@ elements: the diagonal of the matrix.
  -> v a
  -- | Vector of @n-1@ elements, above the diagonal.
  -> v a
  -> Maybe (TriDiagMatrix v a)
triDiagMatrix av bv cv =
  let
    al = DVG.length av
    bl = DVG.length bv
    cl = DVG.length cv
  in if (al == cl) && (al == (bl - 1))
     then Just (TriDiagMatrix av bv cv)
     else Nothing


-- | Create a 'TriDiagMatrix', producing an error at runtime if the dimensions
--   of the vectors are incorrect.
triDiagMatrix'
  :: ( DVG.Vector v a )
  -- | Vector of @n-1@ elements, below the diagonal.
  => v a
  -- | Vector of @n@ elements: the diagonal of the matrix.
  -> v a
  -- | Vector of @n-1@ elements, above the diagonal.
  -> v a
  -> TriDiagMatrix v a
triDiagMatrix' av bv cv =
  case triDiagMatrix av bv cv of
    Nothing ->
      let
        al = DVG.length av
        bl = DVG.length bv
        cl = DVG.length cv
        msg = "(" <>
              "len(av)=" <> show al <> " " <>
              "len(bv)=" <> show bl <> " " <>
              "len(cv)=" <> show cl <>
              ")"
      in error $ "triDiagMatrix' vectors have invalid dimensions " <> msg
    Just m  -> m


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
  :: forall v a.
     ( DVG.Vector v a
     , Fractional a, Epsilon a )
  => TriDiagMatrix v a  -- ^ Tridiagonal matrix, @M@.
  -> v a                -- ^ Known right-hand side, @R@.
  -> Maybe (v a)        -- ^ Solution @U@.
triDiagSolve matrix r =
  triDiagSolve' (as matrix) (bs matrix) (cs matrix) r


-- | Un-sized tridiagonal solver.
--
--   Internally, we just trust that this algorithm gets its indexing correct;
--   hence all the unsafe indexing. C, but in Haskell. :-)
triDiagSolve'
  :: forall v a.
     ( DVG.Vector v a
     , Fractional a, Epsilon a )
  => v a          -- ^ a (one below the diagonal)
  -> v a          -- ^ b (the diagonal)
  -> v a          -- ^ c (one above the diagonal)
  -> v a          -- ^ r (known right-hand-side)
  -> Maybe (v a)  -- ^ u (solution)
triDiagSolve' av bv cv rv =
  let
    -- shortcut for unsafe indexing
    (!) :: DVG.Vector v a => v a -> Int -> a
    (!) = DVG.unsafeIndex

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
          beta_j' = bv!j - (av!(j-1) * gamma_j)

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
    go !x | cont x    = body x >> go (step x)
          | otherwise = pure ()
{-# INLINE cfor #-}
