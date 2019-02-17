{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
module PSO where

-- http://clerc.maurice.free.fr/pso/SPSO_descriptions.pdf

import           Control.Monad               (forM)
import           Control.Monad.Loops         (iterateUntilM, iterateWhile,
                                              unfoldM)
import           Control.Monad.State.Lazy    (MonadState, State)
import qualified Control.Monad.State.Lazy    as State
import           Data.List                   (minimumBy)
import           Data.Ord                    (comparing)
--import           Diagrams.Backend.SVG     (B)
--import qualified Diagrams.Backend.SVG     as SVG
import           Diagrams.Backend.Rasterific (B)
import qualified Diagrams.Backend.Rasterific as BR
import           Diagrams.Prelude            (Diagram, ( # ))
import qualified Diagrams.Prelude            as D
import           System.Random               (Random, RandomGen, mkStdGen,
                                              random, randomR)
import           Viridis                     (viridis)

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

ops2Tuple :: Ops Double (Double, Double)
ops2Tuple
  = Ops
    { zero = (0.0, 0.0)
    , scalarMul = \c (x, y) -> (c * x, c * y)
    , addVec = \(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)
    , subVec = \(x1, y1) (x2, y2) -> (x1 - x2, y1 - y2)
    , radius = \(x, y) -> sqrt (x*x + y*y)
    , fromList = tuple2FromList
    , confine = id
    }
  where
    tuple2FromList :: (Int, [Double] -> (Double, Double))
    tuple2FromList = (2, f)
      where
        f [x, y] = (x, y)
        f _ = error "Can only produce (Double, Double) from 2-element list"


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
  :: forall g m c v. (RandomGen g, MonadState g m, Floating c, Ord c, Random c, Show v, Show c)
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

  initG <- State.get

  let
    step' :: State (Int, [Particle c v], g) (Maybe [Particle c v])
    step' = do
      (n, ps, rng) <- State.get
      if n >= nSteps
        then pure Nothing
        else do
          let (ps', gNext) = State.runState (step params ops fitFn ps) rng
          State.put (n+1, ps', gNext)
          pure (Just ps')

    (pss, (_, _, finalG)) = State.runState (unfoldM step') (0, particles, initG)

  State.put finalG
  pure pss


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

    third :: c
    third = 1.0 / 3.0

    stepParticle :: Particle c v -> m (Particle c v)
    stepParticle particle = do
      u1 <- State.state (randomR (0 :: c, 1 :: c))
      u2 <- State.state (randomR (0 :: c, 1 :: c))
      let
        x = position particle
        v = velocity particle
        p = bestPosition particle
        bestFitn = bestFitness particle
        p' = x ^+^ ((c1 * u1) *^ (p ^-^ x))
        l' = x ^+^ ((c2 * u2) *^ (l ^-^ x))
        g = third *^ (x ^+^ p' ^+^ l')
        r = (radius ops) (g ^-^ x)
      hyp1 <- hyperSample ops
      let
        hyp = (r *^ hyp1) ^+^ g
        v' = (omega *^ v) ^+^ hyp ^-^ x
        x' = (confine ops) $ (x ^+^ v')
        fitn' = fitFn x'
      pure $ Particle
        { position = x'
        , velocity = v'
        , fitness = fitn'
        , bestPosition = if fitn' < bestFitn then x' else p
        , bestFitness = min fitn' bestFitn
        }

stepCanon
  :: forall g m c v. (RandomGen g, MonadState g m, Floating c, Ord c, Random c)
  => Params c                        -- ^ SPO Parameters
  -> Ops c v                         -- ^ Operations
  -> (v -> c)                        -- ^ Fitness function
  -> [Particle c v]                  -- ^ List of particles before the step
  -> m [Particle c v]                -- ^ List of particles after the step
stepCanon params ops fitFn particles = sequence $ fmap stepParticle particles
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
      u1 <- State.state (randomR (0 :: c, 1 :: c))
      u2 <- State.state (randomR (0 :: c, 1 :: c))
      let
        x = position particle
        v = velocity particle
        p = bestPosition particle
        bestFitn = bestFitness particle
        v' = (omega *^ v)
         ^+^ ((c1 * u1) *^ (p ^-^ x))
         ^+^ ((c2 * u2) *^ (l ^-^ x))
        x' = x ^+^ v'
        fitn' = fitFn x'
      pure $ Particle
        { position = x'
        , velocity = v'
        , fitness = fitn'
        , bestPosition = if fitn' < bestFitn then x' else p
        , bestFitness = if fitn' < bestFitn then fitn' else bestFitn
        }

-- | Pick a point strictly inside the unit hypersphere.
hyperSample
  :: forall c g m v. (RandomGen g, MonadState g m, Floating c, Ord c, Random c)
  => Ops c v
  -> m v
hyperSample ops = fl <$> iterateWhile (\xs -> (radiusList xs) > 1.0) boxSample
  where
    (n, fl) = fromList ops
    boxSample = forM [1..n] (const $ State.state (randomR ((-1) :: c, 1 :: c)))
    radiusList xs = sqrt $ sum $ fmap (\x -> x * x) xs


-- | Find the global best position of a list of particles.
globalBestParticle
  :: (Ord c)
  => [Particle c v]
  -> Particle c v
globalBestParticle particles =
  minimumBy (comparing bestFitness) particles


-- | Generate a list of new particles.
newParticles
  :: (RandomGen g, MonadState g m, Fractional c)
  => Ops c v           -- ^ Operations
  -> Int               -- ^ Number of particles
  -> m v               -- ^ Random position function
  -> (v -> c)          -- ^ Fitness function
  -> m [Particle c v]  -- ^ New particles
newParticles ops n posFn fitFn
  = forM [1..n] (const $ newParticle ops posFn fitFn)


-- | Generate one new particle.
newParticle
  :: (RandomGen g, MonadState g m, Fractional c)
  => Ops c v           -- ^ Operations
  -> m v               -- ^ Random position function
  -> (v -> c)          -- ^ Fitness function
  -> m (Particle c v)  -- ^ New particle
newParticle ops posFn fitFn = do
  let
    (*^) = scalarMul ops
  p <- posFn
  v <- fmap (\pv -> 0.2 *^ pv) posFn
  let fitn = fitFn p
  pure Particle
    { position = p
    , velocity = v
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


rastringinFn :: (Double, Double) -> Double
rastringinFn (x, y)
  = 2 * a
  + (x*x - a * cos (2*pi*x))
  + (y*y - a * cos (2*pi*y))
  where
    a = 10


rastringinDia :: Double -> (Int, Int) -> Diagram B
rastringinDia r (nx, ny) = D.rasterDia lfn nx ny # D.sized (D.dims2D (2 * r) (2 * r))
  where
    lfn :: Int -> Int -> D.AlphaColour Double
    lfn x y = dfn x' y'
      where
        x' = r * ((fromIntegral x) / (fromIntegral nx) - 0.5)
        y' = r * ((fromIntegral y) / (fromIntegral ny) - 0.5)

    dfn :: Double -> Double -> D.AlphaColour Double
    dfn x y = D.opaque (viridis $ rastringinFn (x, y) / 55.0)

plotParticles :: [Particle Double (Double, Double)] -> Diagram B
plotParticles ps = (mconcat $ fmap plotParticle ps)
                <> (D.circle 0.2 # D.translateX 0 # D.translateY 0)
  where
    plotParticle :: Particle Double (Double, Double) -> Diagram B
    plotParticle p
      = D.circle 0.1
      # D.translateX (fst . position $ p)
      # D.translateY (snd . position $ p)

rastringinTest :: IO ()
rastringinTest = do
  putStrLn "Rastringin Test"

  let
    bg = rastringinDia 5.12 (100,100)
    randPos = do
      x <- State.state (randomR (-5.12, 5.12))
      y <- State.state (randomR (-5.12, 5.12))
      pure (x, y)
    nSteps = 
    ops = ops2Tuple { confine = \(x,y) ->
                        let
                          x' = if x > 5.12
                               then 5.12
                               else if x < -5.12
                                    then -5.12
                                    else x
                          y' = if y > 5.12
                               then 5.12
                               else if y < -5.12
                                    then -5.12
                                    else y
                        in (x', y')
                        }
    particleAnim = State.evalState (psoAnim defaultParams ops randPos nSteps rastringinFn) (mkStdGen 2)
    dias = fmap (\ps -> (plotParticles ps <> bg)) particleAnim


  _ <- forM particleAnim $ \pset -> do
    let
      gbp = globalBestParticle pset
      optimumPos = bestPosition gbp
      optimumFit = bestFitness gbp

    putStrLn $ "Optimum pos: " <> (show optimumPos)
    putStrLn $ "Optimum fit: " <> (show optimumFit)
    putStrLn $ "  Fits:      " <> show (fmap fitness pset)
    putStrLn $ "  Best Fits: " <> show (fmap bestFitness pset)

  -- SVG.renderSVG "rastringin.svg" (D.mkWidth 800) (rastringinDia 5.12 (500,500))
  -- BR.renderRasterific "rastringin.png" (D.mkWidth 512) dia
  BR.animatedGif "rastringin.gif" (D.dims2D 512 512) BR.LoopingForever 50 dias
