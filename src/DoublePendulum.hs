{-|

Equations from here: https://en.wikipedia.org/wiki/Double_pendulum
-}

{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module DoublePendulum where

import           Codec.Picture                       (Image,
                                                      PixelRGBA8 (PixelRGBA8),
                                                      PixelRGBF)
import qualified Codec.Picture                       as P
import qualified Codec.Picture.Types                 as P
import           Control.Parallel.Strategies         (parMap)
import           Data.AdditiveGroup                  (AdditiveGroup)
import           Data.AffineSpace                    (AffineSpace, Diff, (.+^),
                                                      (.-.))
import           Data.LinearMap                      (linear)
import qualified Data.List.NonEmpty                  as NonEmpty
import           Data.Vector                         (Vector)
import qualified Data.Vector                         as V
import           Data.VectorSpace                    (VectorSpace, (*^))
import           GHC.Generics                        (Generic)
import           Graphics.Rasterific                 (V2 (V2))
import qualified Graphics.Rasterific                 as R
import qualified Graphics.Rasterific.Linear          as RL ((^+^))
import qualified Graphics.Rasterific.Texture         as R
import qualified Graphics.Rasterific.Transformations as R

import qualified ODE                                 as ODE

test :: IO ()
test = do
  let
    rp = RenderParams
         { imgSize = 256
         , bgColor = PixelRGBA8 255 255 255 255
         , segColor = PixelRGBA8 0 0x86 0xc1 255
         , segPixelWidth = 6
         , padFraction = 1.1
         }
    params = Params
             { segMass   = 3.0
             , segLength = 0.2
             , gAccel    = 9.81
             }
    state0 = State { angle1 = pi/2
                   , angle2 = pi/2
                   , momentum1 = 0
                   , momentum2 = 0
                   }

  doublePendulumSim rp params state0 10.0 4 10 "test.gif"


doublePendulumSim
  :: RenderParams  -- ^ Render parameters
  -> Params        -- ^ Double-pendulum parameters
  -> State         -- ^ Initial state
  -> Float         -- ^ Simulation duration (s)
  -> Int           -- ^ Frame period (1/100 s)
  -> Int           -- ^ Number of sub-frames per frame
  -> FilePath      -- ^ Output file path
  -> IO ()         -- ^ IO action
doublePendulumSim rp p state0 tf period subframes filePath = do
  let
    nFrames = ceiling (tf / (0.01 * fromIntegral period))
    nSubFrames = nFrames * subframes
    times = NonEmpty.fromList (ODE.linspace nSubFrames 0 tf)

    gradFn (_, state) = linear $ \dt -> dt *^ grad p state
    states = ODE.integrate ODE.rk4Step state0 times gradFn
    statesVec = V.fromList $ NonEmpty.toList states

    subFrameImages = renderFrame rp p . snd <$> statesVec
    frameImages = averageImages <$> takeGroupN subframes subFrameImages
    rgb8Images = P.convertRGB8 . P.ImageRGBF <$> frameImages

    result = P.writeGifAnimation filePath
                                 period
                                 P.LoopingForever
                                 (V.toList rgb8Images)
  case result of
    Left err -> do
      putStrLn "An error occurred while writing the DoublePendulum gif:"
      putStrLn err
    Right action -> action


grad :: Params -> State -> DState
grad p s =
  let
    c = 6/(segMass p*(segLength p)**2)
    d = -0.5*segMass p*(segLength p)**2
    a = cos(angle1 s - angle2 s)
    b = sin(angle1 s - angle2 s)
    da1 = c*(2*momentum1 s - 3*momentum2 s*a) / (16 - 9*a**2)
    da2 = c*(8*momentum2 s - 3*momentum1 s*a) / (16 - 9*a**2)
  in
    DState
    { dAngle1    = da1
    , dAngle2    = da2
    , dMomentum1 = d*( da1*da2*b + 3*gAccel p/segLength p*sin(angle1 s))
    , dMomentum2 = d*(-da1*da2*b +   gAccel p/segLength p*sin(angle2 s))
    }


{-
renderGif
  :: RenderParams           -- ^ Render parameters
  -> Params                 -- ^ Double-pendulum parameters
  -> Int                    -- ^ Frame period (1/100 s)
  -> Vector (Vector State)  -- ^ Vector containing multiple states for each frame
  -> FilePath               -- ^ Output file path
  -> IO ()                  -- ^ IO action
renderGif rp p period ss filePath = do
  let
    baseImages = fmap (renderFrame rp p) <$> ss
    images = P.convertRGB8 . P.ImageRGBF <$> averageImages <$> baseImages
    result = P.writeGifAnimation filePath
                                 period
                                 P.LoopingForever
                                 (V.toList images)
  case result of
    Left err -> do
      putStrLn "An error occurred while writing the DoublePendulum gif:"
      putStrLn err
    Right action -> action
-}


takeGroupN :: Int -> Vector a -> Vector (Vector a)
takeGroupN n v =
  let
    (cur, remainder) = V.splitAt n v
  in
    if V.length remainder == 0
    then V.singleton cur
    else V.fromList (cur : V.toList (takeGroupN n remainder))


averageImages :: Vector (Image PixelRGBF) -> Image PixelRGBF
averageImages images =
  let
    imgSum :: Image PixelRGBF
    imgSum = foldr1 sumPair images

    sumPair :: Image PixelRGBF -> Image PixelRGBF -> Image PixelRGBF
    sumPair i1 i2 = P.zipPixelComponent3 (\c1 c2 _ -> c1 + c2) i1 i2 i2

    nF :: Float
    nF = fromIntegral (V.length images)

    avg :: Image PixelRGBF
    avg = P.pixelMap (P.colorMap (\c -> c / nF)) imgSum
  in
    avg


renderFrame
  :: RenderParams
  -> Params
  -> State
  -> Image PixelRGBF
renderFrame rp p s =
  let
    scaleFactor = fromIntegral(imgSize rp) / (4*padFraction rp*segLength p)
    center = --fromIntegral(imgSize rp) / 2
      (2*padFraction rp*segLength p)
    xform = R.scale scaleFactor scaleFactor
            <> R.translate (V2 center center)
            <> R.scale 1 -1

    drawSeg p1 p2 = R.stroke (segPixelWidth rp)
                             R.JoinRound
                             (R.CapRound, R.CapRound)
                             (R.line
                                (R.applyTransformation xform p1)
                                (R.applyTransformation xform p2))

    o  = V2 0 0
    e1 = o  RL.^+^ V2 (segLength p*sin(angle1 s)) (-segLength p*cos(angle1 s))
    e2 = e1 RL.^+^ V2 (segLength p*sin(angle2 s)) (-segLength p*cos(angle2 s))

    seg1 = drawSeg o  e1
    seg2 = drawSeg e1 e2
  in
    P.promoteImage
    $ P.dropAlphaLayer
    $ R.renderDrawing (imgSize rp) (imgSize rp) (bgColor rp) $
      R.withTexture (R.uniformTexture (segColor rp)) $ do
        seg1
        seg2


data RenderParams
  = RenderParams
    { imgSize       :: Int
    , bgColor       :: PixelRGBA8
    , segColor      :: PixelRGBA8
    , segPixelWidth :: Float
    , padFraction   :: Float
    }


data Params
  = Params
    { segMass   :: Float
    , segLength :: Float
    , gAccel    :: Float
    }

data State
  = State
    { angle1    :: Float
    , angle2    :: Float
    , momentum1 :: Float
    , momentum2 :: Float
    } deriving (Show)

data DState
  = DState
    { dAngle1    :: Float
    , dAngle2    :: Float
    , dMomentum1 :: Float
    , dMomentum2 :: Float
    } deriving (Show, Generic, AdditiveGroup, VectorSpace)


instance AffineSpace State where
  type Diff State = DState
  s1 .-. s2 = DState
              { dAngle1    = angle1    s1 - angle1    s2
              , dAngle2    = angle2    s1 - angle2    s2
              , dMomentum1 = momentum1 s1 - momentum1 s2
              , dMomentum2 = momentum2 s1 - momentum2 s2
              }
  s .+^ ds = State
             { angle1    = angle1    s + dAngle1    ds
             , angle2    = angle2    s + dAngle2    ds
             , momentum1 = momentum1 s + dMomentum1 ds
             , momentum2 = momentum2 s + dMomentum2 ds
             }
