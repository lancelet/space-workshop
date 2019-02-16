{-# LANGUAGE ScopedTypeVariables #-}
module PSO where

-- http://clerc.maurice.free.fr/pso/SPSO_descriptions.pdf

import Control.Monad (forM)
import Control.Monad.Loops (iterateWhile)
import           Control.Monad.State.Lazy (MonadState)
import qualified Control.Monad.State.Lazy as State
import           Data.List                (maximumBy)
import           Data.Ord                 (comparing)
import           System.Random            (RandomGen, mkStdGen, random, Random)

data Particle c v
  = Particle
    { position     :: !v
    , velocity     :: !v
    , fitness      :: !c
    , bestPosition :: !v
    , bestFitness  :: !c
    } deriving (Show)


data Ops c v
  = Ops
    { zero      :: v
    , scalarMul :: c -> v -> v
    , addVec    :: v -> v -> v
    , fromList  :: (Int, [c] -> v) }

opsDouble :: Ops Double Double
opsDouble
  = Ops
    { zero = 0.0
    , scalarMul = \c x -> c * x
    , addVec = \x y -> x + y
    , fromList = doubleFromList }
  where
    doubleFromList :: (Int, [Double] -> Double)
    doubleFromList = (1, f)
      where
        f [x] = x
        f _ = error "Can only produce a Double from a single-element list!"


-- | Pick a point strictly inside the unit hypersphere.
hyperSample
  :: (RandomGen g, MonadState g m, Floating c, Ord c, Random c)
  => Ops c v
  -> m v
hyperSample ops = fl <$> iterateWhile (\xs -> (radius xs) < 1.0) boxSample
  where
    (n, fl) = fromList ops
    boxSample = forM [1..n] (const $ State.state random)
    radius xs = sqrt $ sum $ fmap (** 2) xs


-- | Find the global best position of a list of particles.
globalBestPosition
  :: (Ord c)
  => [Particle c v]
  -> v
globalBestPosition particles =
  bestPosition $ maximumBy (comparing bestFitness) particles



-- | Generate a list of new particles.
newParticles
  :: (RandomGen g, MonadState g m)
  => Ops c v           -- ^ Operations
  -> Int               -- ^ Number of particles
  -> m v               -- ^ Random position function
  -> (v -> c)          -- ^ Fitness function
  -> m [Particle c v]  -- ^ New particles
newParticles ops n posFn fitFn
  = forM [1..n] (const $ newParticle ops posFn fitFn)
  

-- | Generate one new particle.
newParticle
  :: (RandomGen g, MonadState g m)
  => Ops c v           -- ^ Operations
  -> m v               -- ^ Random position function
  -> (v -> c)          -- ^ Fitness function
  -> m (Particle c v)  -- ^ New particle
newParticle ops posFn fitFn = do
  p <- posFn
  let fitn = fitFn p
  pure Particle
    { position = p
    , velocity = zero ops
    , fitness = fitn
    , bestPosition = p
    , bestFitness = fitn
    }


testNewParticles :: [Particle Double Double]
testNewParticles = State.evalState (newParticles opsDouble 5 posFn (const 1)) s0
  where
    s0 = mkStdGen 0

    posFn :: (RandomGen g, MonadState g m) => m Double
    posFn = State.state random
  
