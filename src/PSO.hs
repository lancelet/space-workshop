{-# LANGUAGE ScopedTypeVariables #-}
module PSO where

-- http://clerc.maurice.free.fr/pso/SPSO_descriptions.pdf

import           Control.Monad            (forM)
import           Control.Monad.Loops      (iterateWhile, iterateUntilM)
import           Control.Monad.State.Lazy (MonadState)
import qualified Control.Monad.State.Lazy as State
import           Data.List                (maximumBy)
import           Data.Ord                 (comparing)
import           System.Random            (Random, RandomGen, mkStdGen, random, randomR)

data Particle c v
  = Particle
    { position     :: !v
    , velocity     :: !v
    , fitness      :: !c
    , bestPosition :: !v
    , bestFitness  :: !c
    } deriving (Show)

data Params c
  = Params
    { cognitiveAccelCoeff :: c
    , socialAccelCoeff    :: c
    , inertiaWeight       :: c
    , objectiveFitness    :: c
    , maxIterations       :: Int
    , nParticles          :: Int
    }

defaultParams :: (Floating c) => Params c
defaultParams = Params
  { cognitiveAccelCoeff = 0.5 + log 2.0
  , socialAccelCoeff    = 0.5 + log 2.0
  , inertiaWeight       = 1.0 / (2.0 * log 2.0)
  , objectiveFitness    = 0.01
  , maxIterations       = 100
  , nParticles          = 40
  }

data Ops c v
  = Ops
    { zero      :: v
    , scalarMul :: c -> v -> v
    , addVec    :: v -> v -> v
    , subVec    :: v -> v -> v
    , radius    :: v -> c
    , fromList  :: (Int, [c] -> v)
    , confine   :: v -> v }

opsDouble :: Ops Double Double
opsDouble
  = Ops
    { zero = 0.0
    , scalarMul = \c x -> c * x
    , addVec = \x y -> x + y
    , subVec = \x y -> x - y
    , radius = id
    , fromList = doubleFromList
    , confine = id }
  where
    doubleFromList :: (Int, [Double] -> Double)
    doubleFromList = (1, f)
      where
        f [x] = x
        f _   = error "Can only produce a Double from a single-element list!"


pso
  :: forall g m c v. (RandomGen g, MonadState g m, Floating c, Ord c, Random c)
  => Params c
  -> Ops c v
  -> m v
  -> (v -> c)
  -> m (v, c, Int)
pso params ops randPos fitFn = do
  -- Init
  particles <- newParticles ops (nParticles params) randPos fitFn
  -- Run until we terminate
  let
    shouldTerminate :: ([Particle c v], Int) -> Bool
    shouldTerminate (_, n) | n > (maxIterations params) = True
    shouldTerminate (ps, _) | bestFitness (globalBestParticle ps) <= (objectiveFitness params) = True
    shouldTerminate _ = False

    action :: ([Particle c v], Int) -> m ([Particle c v], Int)
    action (ps, n) = do
      ps' <- step params ops fitFn ps
      pure (ps', n + 1)
  
  (particles', n) <- iterateUntilM shouldTerminate action (particles, 0)
  
  let
    globalBest = globalBestParticle particles'
  
  pure (bestPosition globalBest, bestFitness globalBest, n)


psoAnim
  :: forall g m c v. (RandomGen g, MonadState g m, Floating c, Ord c, Random c)
  => Params c
  -> Ops c v
  -> m v
  -> Int
  -> (v -> c)
  -> m ([[Particle c v]])
psoAnim params ops randPos nSteps fitFn = do
  -- Init
  particles <- newParticles ops (nParticles params) randPos fitFn
  -- Run until we terminate
  let
    shouldTerminate :: ([[Particle c v]], Int) -> Bool
    shouldTerminate (_, n) | n >= nSteps = True
    shouldTerminate _ = False

    action :: ([[Particle c v]], Int) -> m ([[Particle c v]], Int)
    action (pps, n) = do
      ps' <- step params ops fitFn (head pps)
      pure (ps' : pps, n + 1)

  steps :: ([[Particle c v]], Int) <- iterateUntilM shouldTerminate action ([particles], nSteps)

  pure (reverse $ fst steps)


step
  :: forall g m c v. (RandomGen g, MonadState g m, Floating c, Ord c, Random c)
  => Params c                        -- ^ SPO Parameters
  -> Ops c v                         -- ^ Operations
  -> (v -> c)                        -- ^ Fitness function
  -> [Particle c v]                  -- ^ List of particles before the step
  -> m [Particle c v]                -- ^ List of particles after the step
step params ops fitFn particles = sequence $ fmap stepParticle particles
  where
    gbp :: Particle c v
    gbp = globalBestParticle particles
    l = bestPosition gbp

    (^+^) = addVec ops
    (^-^) = subVec ops
    (*^) = scalarMul ops
    infixl 6 ^+^, ^-^
    infixl 7 *^

    c1 = cognitiveAccelCoeff params
    c2 = socialAccelCoeff params
    omega = inertiaWeight params
    
    stepParticle :: Particle c v -> m (Particle c v)
    stepParticle particle = do
      u1 <- State.state (randomR (0, 1))
      u2 <- State.state (randomR (0, 1))
      let
        x = position particle
        v = velocity particle
        p = bestPosition particle
        bestFitn = bestFitness particle
        p' = x ^+^ (c1 * u1) *^ (p ^-^ x)
        l' = x ^+^ (c2 * u2) *^ (l ^-^ x)
        g = (1.0/3.0) *^ (x ^+^ p' ^+^ l')
        r = (radius ops) g
      hyp1 <- hyperSample ops
      let
        hyp = r *^ hyp1 ^+^ g
        v' = omega *^ v ^+^ (hyp ^-^ x)
        x' = (confine ops) $ x ^+^ v'
        fitn' = fitFn x'
      pure $ Particle
        { position = x'
        , velocity = v'
        , fitness = fitn'
        , bestPosition = if fitn' < bestFitn then x' else x
        , bestFitness = min fitn' bestFitn
        }
  

-- | Pick a point strictly inside the unit hypersphere.
hyperSample
  :: (RandomGen g, MonadState g m, Floating c, Ord c, Random c)
  => Ops c v
  -> m v
hyperSample ops = fl <$> iterateWhile (\xs -> (radiusList xs) < 1.0) boxSample
  where
    (n, fl) = fromList ops
    boxSample = forM [1..n] (const $ State.state random)
    radiusList xs = sqrt $ sum $ fmap (** 2) xs


-- | Find the global best position of a list of particles.
globalBestParticle
  :: (Ord c)
  => [Particle c v]
  -> Particle c v
globalBestParticle particles =
  maximumBy (comparing bestFitness) particles


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


