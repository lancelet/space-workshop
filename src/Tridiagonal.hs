{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
module Tridiagonal where

import           Control.Lens                      ((^.))
import           Control.Lens.TH                   (makeLenses)
import           Control.Monad.Except              (runExceptT, throwError, ExceptT, when)
import           Control.Monad.ST                  (ST)
import qualified Control.Monad.ST                  as ST
import           Control.Monad.Trans.Class         (lift)
import qualified Data.STRef                        as STRef
import           GHC.TypeNats                      (type (-), KnownNat, Nat, natVal)
import           Data.Proxy                        (Proxy(Proxy))
import qualified Data.Vector.Generic               as DVG
import qualified Data.Vector.Generic.Mutable       as DVGM
import qualified Data.Vector.Generic.Mutable.Sized as DVGMS
import qualified Data.Vector.Generic.Sized         as DVGS
import qualified Data.Vector.Sized                 as DVS
import           Linear.Epsilon                    (Epsilon, nearZero)


data TriDiagMatrix v (n :: Nat) a
  = TriDiagMatrix
    { _as :: DVGS.Vector v (n-1) a
    , _bs :: DVGS.Vector v    n  a
    , _cs :: DVGS.Vector v (n-1) a
    }
makeLenses ''TriDiagMatrix


-- | Solve a TriDiagMatrix linear system in O(N).
triDiagSolve
  :: forall v n a.
     ( DVG.Vector v a
     , KnownNat n
     , Fractional a, Epsilon a )
  => TriDiagMatrix v n a
  -> DVGS.Vector v n a
  -> Maybe (DVGS.Vector v n a)
triDiagSolve matrix r =
  let

    action :: ExceptT () (ST s) (DVGS.Vector v n a)
    action = do
      let
        nn = fromIntegral (natVal (Proxy :: Proxy n)) :: Int
        av = DVGS.unsafeIndex (matrix^.as)
        bv = DVGS.unsafeIndex (matrix^.bs)
        cv = DVGS.unsafeIndex (matrix^.cs)
        rv = DVGS.unsafeIndex r
      bet <- lift $ STRef.newSTRef (bv 0)
      uv  <- lift $ DVGMS.new
      gam <- lift $ DVGMS.clone uv

      when (nearZero (bv 0)) (throwError ())
      lift $ DVGMS.unsafeWrite uv 0 ((rv 0) / (bv 0))
  
      -- Decomposition and forward substitution
      cfor 1 (< nn) (+ 1) $ \j -> do
        betj <- lift $ STRef.readSTRef bet
        ujn1 <- lift $ DVGMS.unsafeRead uv (j - 1)
        let
          gamj  = cv (j - 1) / betj
          betj' = bv j - (av (j - 1)) * gamj
        when (nearZero betj') (throwError ())
        let
          uj = (rv j - (av (j - 1)) * ujn1) / betj'
        lift $ DVGMS.unsafeWrite gam j gamj
        lift $ STRef.writeSTRef bet betj'
        lift $ DVGMS.unsafeWrite uv j uj
  
      -- Backsubstitution
      cfor (nn - 2) (>= 0) (\x -> x - 1) $ \j -> do
        uj     <- lift $ DVGMS.unsafeRead uv j
        ujp1   <- lift $ DVGMS.unsafeRead uv (j + 1)
        gamjp1 <- lift $ DVGMS.unsafeRead gam (j + 1)
        let
          uj' = uj - gamjp1 * ujp1
        lift $ DVGMS.unsafeWrite uv j uj'
 
      lift $ DVGS.freeze uv

    either2Maybe :: Either () b -> Maybe b
    either2Maybe (Left ()) = Nothing
    either2Maybe (Right x) = Just x
    
  in
     either2Maybe $ ST.runST $ runExceptT $ action



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
  

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = error "NOT JUST"
  

example =
  let
    av :: DVS.Vector 5 Double
    av = fromJust $ DVS.fromList [4, 2, 5, 9, 4]

    bv :: DVS.Vector 6 Double
    bv = fromJust $ DVS.fromList [3, 1, 6, 8, 3, 4]

    cv :: DVS.Vector 5 Double
    cv = fromJust $ DVS.fromList [1, 5, 5, 9, 2]

    rv :: DVS.Vector 6 Double
    rv = fromJust $ DVS.fromList [1, 2, 3, 4, 5, 6]

    matrix = TriDiagMatrix av bv cv
   
  in triDiagSolve matrix rv

  
{-
triDiagSolve'
  :: forall v a.
     ( Fractional a
     , DVG.Vector v a)
  => v a  -- ^ as
  -> v a  -- ^ bs
  -> v a  -- ^ cs
  -> v a  -- ^ rs
  -> v a  -- ^ us
triDiagSolve' as bs cs rs = ST.runST $ do
  bet <- STRef.newSTRef (bs DVG.! 0)
  uv <- DVGM.new (DVG.length rs)
  DVGM.set uv ((rs DVG.! 0) / (bs DVG.! 0))
  DVG.freeze uv
-}

{-
triDiagSolve
  :: forall v n m a.
     ( KnownNat n
     , PrimMonad m
     , MV.MVector v a )
  => TriDiagMatrix v n a
  -> Vector v n a
  -> m (MVector v n (PrimState m) a)
triDiagSolve = undefined
-}
  

{-
triDiagSolve
  :: forall a. (Fractional a)
  => [a]  -- ^ a values  (no a0)
  -> [a]  -- ^ b values
  -> [a]  -- ^ c values  (no an)
  -> [a]  -- ^ r values (known RHS)
  -> [a]  -- ^ solution
triDiagSolve as bs cs rs =
  let
    b0 :: a
    b0 = head bs

    r0 :: a
    r0 = head rs

    trd :: (a, b, c) -> c
    trd (_, _, x) = x

    fwr :: [(a, a, a)]
    fwr = fwdSubst b0 r0 (zip4 as (tail bs) cs (tail rs))

    rfwr = reverse fwr

  in
    reverse (fmap fst (backSubst (trd (head rfwr), case (head rfwr) of (_, x, _) -> x) (tail rfwr)))


fwdSubst :: forall a. (Fractional a) => a -> a -> [(a, a, a, a)] -> [(a, a, a)]
fwdSubst b0 r0 = scanl fwdStep (b0, 0, r0/b0)
  where
    fwdStep :: (a, a, a) -> (a, a, a, a) -> (a, a, a)
    fwdStep (bet, _, u) (aj, bj, cjp, rj) =
      let
        gam' = cjp / bet
        bet' = bj - aj*gam'
        u'   = (rj - aj*u)/bet'
      in
        (bet', gam', u')


backSubst :: forall a. (Fractional a) => (a, a) -> [(a, a, a)] -> [(a, a)]
backSubst = scanl backStep
  where
    backStep :: (a, a) -> (a, a, a) -> (a, a)
    backStep (u, gam) (_, gam', uu) = (uu - (gam * u), gam')


example =
  let
    as :: [Double]
    as = [ 4, 2, 5, 9, 4 ]

    bs :: [Double]
    bs = [ 3, 1, 6, 8, 3, 4 ]

    cs :: [Double]
    cs = [1, 5, 5, 9, 2 ]

    rs :: [Double]
    rs = [1, 2, 3, 4, 5, 6]

  in
    -- fwdSubst (head bs) (head rs) (zip4 as (tail bs) cs (tail rs))
    triDiagSolve as bs cs rs
-}
