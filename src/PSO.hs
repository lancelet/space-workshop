{-# LANGUAGE ScopedTypeVariables #-}
module PSO where

-- http://clerc.maurice.free.fr/pso/SPSO_descriptions.pdf

import Control.Monad (forM)
import           Control.Monad.State.Lazy (MonadState)
import qualified Control.Monad.State.Lazy as State
import           Data.List                (maximumBy)
import qualified Data.List.NonEmpty       as NonEmpty
import           Data.Ord                 (comparing)
import           System.Random            (RandomGen, mkStdGen, random)

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
    , addVec    :: v -> v -> v }

opsDouble :: Ops Double Double
opsDouble
  = Ops
    { zero = 0.0
    , scalarMul = \c x -> c * x
    , addVec = \x y -> x + y }



{-
step
  :: (Ord c, RandomGen g)
  => Ops c v              -- ^ Operations
  -> g                    -- ^ Initial random state
  -> [Particle c v]       -- ^ Input list of particles
  -> ([Particle c v], g)  -- ^ Updated particles and output random state
step ops gen particles = undefined
-}


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
  position <- posFn
  let fitness = fitFn position
  pure Particle
    { position = position
    , velocity = zero ops
    , fitness = fitness
    , bestPosition = position
    , bestFitness = fitness
    }


testNewParticles :: [Particle Double Double]
testNewParticles = State.evalState (newParticles opsDouble 5 posFn (const 1)) s0
  where
    s0 = mkStdGen 0

    posFn :: (RandomGen g, MonadState g m) => m Double
    posFn = State.state random
  
