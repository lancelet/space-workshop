{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module ODE where

import Data.Ratio ((%))
import qualified Data.Vector.Generic       as V
import qualified Data.Vector.Generic.Sized as Sized

data EventAction
  = Continue
  | Terminate

data Event v n a
  = Event
    { action       :: EventAction
    , testFunction :: (a, Sized.Vector v n a) -> a
    }

-- | Computes a single step of Euler integration.
eulerStep
  :: forall a n v. (V.Vector v a, Num a)
  => a
  -- ^ size of the time step
  -> a
  -- ^ time at the start of the step
  -> Sized.Vector v n a
  -- ^ state vector at the start of the step
  -> ((a, Sized.Vector v n a) -> Sized.Vector v n a)
  -- ^ function from time and state vector to gradient vector
  -> Sized.Vector v n a
  -- ^ state vector after the step
eulerStep h t yv fv = Sized.zipWith scalarEulerStep yv (fv (t, yv))
  where
    scalarEulerStep :: a -> a -> a
    scalarEulerStep y dvdt = y + h * dvdt
{-# INLINE eulerStep #-}


-- | Add two vectors element-wise.
vadd
  :: forall a n v. (V.Vector v a, Num a)
  => Sized.Vector v n a
  -> Sized.Vector v n a
  -> Sized.Vector v n a
vadd = Sized.zipWith (+)
{-# INLINE vadd #-}


-- | Subtract one vector from another element-wise.
vsub
  :: forall a n v. (V.Vector v a, Num a)
  => Sized.Vector v n a
  -> Sized.Vector v n a
  -> Sized.Vector v n a
vsub = Sized.zipWith (-)
{-# INLINE vsub #-}


-- | Scale a vector by a scalar coefficient.
vscale
  :: forall a n v. (V.Vector v a, Num a)
  => a
  -> Sized.Vector v n a
  -> Sized.Vector v n a
vscale s = Sized.map ((*) s)
{-# INLINE vscale #-}


data Rk45StepResult v n a
  = Rk45StepResult
    { state :: Sized.Vector v n a
    , err   :: Sized.Vector v n a
    }


-- | Computes a single step of RK45.
--
--   (4th order Runge-Kutta embedded in a 5th order evaluation for the purpose
--   of error estimation.)
rk45Step
  :: forall a n v. (V.Vector v a, Num a, Fractional a)
  => a
  -- ^ size of the time step
  -> a
  -- ^ time at the start of the step
  -> Sized.Vector v n a
  -- ^ state vector at the start of the step
  -> ((a, Sized.Vector v n a) -> Sized.Vector v n a)
  -- ^ function from time and state vector to gradient vector
  -> Rk45StepResult v n a
rk45Step h t yv fv =
  let
    -- c constant coefficients
    c2 = fromRational $ 1 % 5
    c3 = fromRational $ 3 % 10
    c4 = fromRational $ 4 % 5
    c5 = fromRational $ 8 % 9
    c6 = 1.0
    -- aij constant coefficients
    a21 = fromRational $ 1 % 5
    a31 = fromRational $ 3 % 40
    a41 = fromRational $ 44 % 45
    a51 = fromRational $ 19372 % 6561
    a61 = fromRational $ 9017 % 3168
    a32 = fromRational $ 9 % 40
    a42 = fromRational $ (-56) % 15
    a52 = fromRational $ (-25360) % 2187
    a62 = fromRational $ (-355) % 33
    a43 = fromRational $ 32 % 9
    a53 = fromRational $ 64448 % 6561
    a63 = fromRational $ 46732 % 5247
    a54 = fromRational $ (-212) % 729
    a64 = fromRational $ 49 % 176
    a65 = fromRational $ (-5103) % 18656
    -- bi constant coefficients
    b1 = fromRational $ 35 % 384
    -- b2 = 0.0  (don't actually multiply the zeros)
    b3 = fromRational $ 500 % 1113
    b4 = fromRational $ 125 % 192
    b5 = fromRational $ (-2187) % 6784
    b6 = fromRational $ 11 % 84
    -- bistar constant coefficients
    bs1 = fromRational $ 5179 % 57600
    -- bs2 = 0.0  (don't actually multiply the zeros)
    bs3 = fromRational $ 7571 % 16695
    bs4 = fromRational $ 393 % 640
    bs5 = fromRational $ (-92097) % 339200
    bs6 = fromRational $ 187 % 2100
  in
    let
      k1 = h `vscale` fv (t, yv)
      k2 = h `vscale` fv (t + c2 * h, yv `vadd` (a21 `vscale` k1))
      k3 = h `vscale` fv (t + c3 * h, yv `vadd` (a31 `vscale` k1)
                                         `vadd` (a32 `vscale` k2))
      k4 = h `vscale` fv (t + c4 * h, yv `vadd` (a41 `vscale` k1)
                                         `vadd` (a42 `vscale` k2)
                                         `vadd` (a43 `vscale` k3))
      k5 = h `vscale` fv (t + c5 * h, yv `vadd` (a51 `vscale` k1)
                                         `vadd` (a52 `vscale` k2)
                                         `vadd` (a53 `vscale` k3)
                                         `vadd` (a54 `vscale` k4))
      k6 = h `vscale` fv (t + c6 * h, yv `vadd` (a61 `vscale` k1)
                                         `vadd` (a62 `vscale` k2)
                                         `vadd` (a63 `vscale` k3)
                                         `vadd` (a64 `vscale` k4)
                                         `vadd` (a65 `vscale` k5))
      ynext = yv
              `vadd` (b1 `vscale` k1)
              -- `vadd` (b2 `vscale` k2)  -- b2 is zero
              `vadd` (b3 `vscale` k3)
              `vadd` (b4 `vscale` k4)
              `vadd` (b5 `vscale` k5)
              `vadd` (b6 `vscale` k6)
      ynexts = yv
               `vadd` (bs1 `vscale` k1)
               -- `vadd` (bs2 `vscale` k2)  -- bs2 is zero
               `vadd` (bs3 `vscale` k3)
               `vadd` (bs4 `vscale` k4)
               `vadd` (bs5 `vscale` k5)
               `vadd` (bs6 `vscale` k6)
    in 
        Rk45StepResult
        { state = ynext
        , err   = ynext `vsub` ynexts
        }
