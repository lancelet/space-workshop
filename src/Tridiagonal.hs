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
import           GHC.TypeNats                      (type (-), KnownNat, Nat,
                                                    natVal)
import           Data.Proxy                        (Proxy(Proxy))
import qualified Data.STRef                        as STRef
import qualified Data.Vector.Generic               as DVG
import qualified Data.Vector.Generic.Mutable.Sized as DVGMS
import qualified Data.Vector.Generic.Sized         as DVGS
import qualified Data.Vector.Unboxed               as DVU
import qualified Data.Vector.Unboxed.Sized         as DVUS
import           Linear.Epsilon                    (Epsilon, nearZero)


-- | Tridiagonal (square) matrix of size @n x n@ elements.
data TriDiagMatrix v (n :: Nat) a
  = TriDiagMatrix
    { -- | @as@ are the elements 1 below the diagonal.
      _as :: DVGS.Vector v (n-1) a
      -- | @bs@ are the diagonal elements.
    , _bs :: DVGS.Vector v    n  a
      -- | @cs@ are the elements 1 above the diagonal.
    , _cs :: DVGS.Vector v (n-1) a
    }

-- | Lens for the @as@ field (1 below the diagonal) of a 'Tridiagonal' matrix.
as :: forall v n a. Lens' (TriDiagMatrix v n a) (DVGS.Vector v (n-1) a)
as = lens _as (\m _as' -> m {_as = _as'})

-- | Lens for the @bs@ field (diagonal) of a 'Tridiagonal' matrix.
bs :: forall v n a. Lens' (TriDiagMatrix v n a) (DVGS.Vector v n a)
bs = lens _bs (\m _bs' -> m {_bs = _bs'})

-- | Lens for the @cs@ field (1 above the diagonal) of a 'Tridiagonal' matrix.
cs :: forall v n a. Lens' (TriDiagMatrix v n a) (DVGS.Vector v (n-1) a)
cs = lens _cs (\m _cs' -> m {_cs = _cs'})
  

-- | Solve a 'TriDiagMatrix' linear system in O(N).
--
--   Solves @M * U = R@ for @U@ given @M@ and @R@.
--
--   The only type of failure possible in the solver is a zero (or near-zero)
--   pivot element.
--
--   The algorithm used here is the O(N) solver described in:
--
--     * Press et al. (2007) Numerical Recipes. The Art of Scientific Computing.
--         Cambridge University Press.
--         (Solution of Linear Systems -> Tridiagonal and Band-Diagonal Systems
--          of Equations)
--
--   The solver itself executes in the 'ST' monad. It allocates a single
--   mutable vector that becomes the result @U@, and an accumulator of type
--   @a@.
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

    -- Alias for unsafe index.
    (!) :: forall m. DVGS.Vector v m a -> Int -> a
    vec ! i = DVGS.unsafeIndex vec i
    infix 7 !

    -- Aliases for reading and writing mutable values.
    vWrite vec index value = lift (DVGMS.unsafeWrite vec index value)
    vRead vec index = lift (DVGMS.unsafeRead vec index)
    sRead ref = lift (STRef.readSTRef ref)
    sWrite ref value = lift (STRef.writeSTRef ref value)

    -- action runs in ST for mutation, using ExceptT to provide shortcut
    --  termination in the error case of a zero pivot element
    action :: forall s. ExceptT () (ST s) (DVGS.Vector v n a)
    action = do
      let nn = fromIntegral (natVal (Proxy :: Proxy n)) :: Int
      bet <- lift $ STRef.newSTRef (matrix^.bs ! 0)
      uv  <- lift $ DVGMS.new
      gam <- lift $ DVGMS.clone uv

      when (nearZero (matrix^.bs ! 0)) (throwError ())
      vWrite uv 0 ((r ! 0) / (matrix^.bs ! 0))
  
      -- Decomposition and forward substitution
      cfor 1 (< nn) (+ 1) $ \j -> do
        bet_j <- sRead bet
        u_jn1 <- vRead uv (j - 1)
        let
          gam_j  = (matrix^.cs ! (j - 1)) / bet_j
          bet_j' = ((matrix^.bs ! j) - (matrix^.as ! (j - 1))) * gam_j
        when (nearZero bet_j') (throwError ())
        let u_j = (r ! j - (matrix^.as ! (j - 1)) * u_jn1) / bet_j'
        vWrite gam j gam_j
        sWrite bet bet_j'
        vWrite uv j u_j
  
      -- Backsubstitution
      cfor (nn - 2) (>= 0) (\x -> x - 1) $ \j -> do
        u_j     <- vRead uv j
        u_jp1   <- vRead uv (j + 1)
        gam_jp1 <- vRead gam (j + 1)
        let u_j' = u_j - gam_jp1 * u_jp1
        vWrite uv j u_j'
 
      lift $ DVGS.freeze uv

    -- Convert an Either to a Maybe.
    either2Maybe :: Either () b -> Maybe b
    either2Maybe (Left ()) = Nothing
    either2Maybe (Right x) = Just x
    
  in either2Maybe $ ST.runST $ runExceptT $ action

{-# SPECIALIZE triDiagSolve
    :: ( KnownNat n )
    => TriDiagMatrix DVU.Vector n Double
    -> DVUS.Vector n Double
    -> Maybe (DVUS.Vector n Double) #-}
{-# SPECIALIZE triDiagSolve
    :: ( KnownNat n )
    => TriDiagMatrix DVU.Vector n Float
    -> DVUS.Vector n Float
    -> Maybe (DVUS.Vector n Float) #-}


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
