{-# LANGUAGE ScopedTypeVariables #-}
module PSO where

-- http://clerc.maurice.free.fr/pso/SPSO_descriptions.pdf

import System.Random (RandomGen)
import qualified Data.List.NonEmpty as NonEmpty

data Particle c v
  = Particle
    { position     :: !v
    , velocity     :: !v
    , fitness      :: !c
    , bestPosition :: !v
    , bestFitness  :: !c
    } deriving (Show)


data Ops v
  = Ops
    { zero :: v
    }

opsDouble :: Ops Double
opsDouble
  = Ops
    { zero = 0.0 }



  
-- TODO: Refactor this; unfoldr is adding a lot of complexity.
newParticles
  :: forall g c v.
     (RandomGen g)
  => Ops v                -- ^ Operations
  -> g                    -- ^ Initial random state
  -> Int                  -- ^ Number of particles
  -> (g -> (v, g))        -- ^ Random position function
  -> (v -> c)             -- ^ Fitness function
  -> ([Particle c v], g)  -- ^ New particles
newParticles ops gen n posFn fitFn
  = (fmap fst uf, (snd . last) uf)

  where

    uf :: [(Particle c v, g)]
    uf = NonEmpty.toList $ NonEmpty.unfoldr ufun (gen, n)

    ufun :: (g, Int) -> ((Particle c v, g), Maybe (g, Int))
    ufun (curGen, i) = ((particle, nextGen), maybeNext)
      where
        (particle, nextGen) = newParticle ops curGen posFn fitFn
        maybeNext = if i > 1
                    then Just (nextGen, i - 1)
                    else Nothing
  
  
-- | Generate one new particle.
newParticle
  :: (RandomGen g)
  => Ops v              -- ^ Operations
  -> g                  -- ^ Initial random state
  -> (g -> (v, g))      -- ^ Random position function
  -> (v -> c)           -- ^ Fitness function
  -> (Particle c v, g)  -- ^ New particle
newParticle ops gen posFn fitFn
  = (particle, gen')
  where
    particle = Particle
               { position = p
               , velocity = zero ops
               , fitness = f
               , bestPosition = p
               , bestFitness = f 
               }
    (p, gen') = posFn gen
    f = fitFn p
