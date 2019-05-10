module Main where

import           Codec.Picture       (PixelRGBA8 (PixelRGBA8))
import           Data.Bits           (shift, (.&.))
import           Data.Word           (Word8)
import           Options.Applicative ((<**>))
import qualified Options.Applicative as A

import qualified DoublePendulum      as DP

main :: IO ()
main = render =<< A.execParser opts
  where
    opts
      = A.info (optionsParser <**> A.helper)
        (A.fullDesc
         <> A.progDesc "Renders a simple double-pendulum animation"
         <> A.header "dpendulum - render a double-pendulum gif")


data InputColor = InputColor !Word8 !Word8 !Word8

instance Show InputColor where
  show (InputColor r g b) = toHex r <> toHex g <> toHex b


toPixelRGBA8 :: InputColor -> PixelRGBA8
toPixelRGBA8 (InputColor r g b) = PixelRGBA8 r g b 0xFF


white :: InputColor
white = InputColor 255 255 255


lightBlue :: InputColor
lightBlue = InputColor 0x00 0x86 0xC1


lightGray :: InputColor
lightGray = InputColor 0xC1 0xC1 0xC1


data Options
  = Options
    { duration   :: Float
    , gifPeriod  :: Int
    , subFrames  :: Int
    , size       :: Int
    , background :: InputColor
    , link       :: InputColor
    , linkWidth  :: Float
    , trace      :: Bool
    , traceColor :: InputColor
    , traceWidth :: Float
    , traceLimit :: Maybe Float
    , filePath   :: String
    , angle0     :: Float
    , angle1     :: Float
    }


optionsToRenderParams :: Options -> DP.RenderParams
optionsToRenderParams o =
  DP.RenderParams
  { DP.imgSize = size o
  , DP.bgColor = toPixelRGBA8 $ background o
  , DP.segColor = toPixelRGBA8 $ link o
  , DP.segPixelWidth = linkWidth o
  , DP.padFraction = 1.1
  , DP.traceColor = toPixelRGBA8 $ traceColor o
  , DP.tracePixelWidth = if trace o then Just (traceWidth o) else Nothing
  , DP.traceSubframes = (\t -> ceiling (t/fromIntegral (gifPeriod o)*100
                                          *fromIntegral (subFrames o))) <$> traceLimit o
  }
  

optionsToState :: Options -> DP.State
optionsToState o =
  DP.State
  { DP.angle1 = pi / 180 * angle0 o
  , DP.angle2 = pi / 180 * angle1 o
  , DP.momentum1 = 0
  , DP.momentum2 = 0
  }


defaultParams :: DP.Params
defaultParams =
  DP.Params
  { DP.segMass   = 3.0  -- kg
  , DP.segLength = 0.5  -- m
  , DP.gAccel    = 9.81 -- m/s/s
  }


render :: Options -> IO ()
render o = 
  let
    rp = optionsToRenderParams o
    state0 = optionsToState o
  in
    DP.doublePendulumSim rp defaultParams state0
                         (duration o)
                         (gifPeriod o)
                         (subFrames o)
                         (filePath o)


optionsParser :: A.Parser Options
optionsParser
  = Options
    <$> A.option A.auto
        (A.long "duration"
         <> A.short 'd'
         <> A.help "Duration (seconds)"
         <> A.showDefault
         <> A.value 5.0
         <> A.metavar "Float")
    <*> A.option A.auto
        (A.long "period"
         <> A.short 'p'
         <> A.help "Period of gif frames (1/100 seconds)"
         <> A.showDefault
         <> A.value 4
         <> A.metavar "INT")
    <*> A.option A.auto
        (A.long "sub-frames"
         <> A.help "Sub-frames per frame"
         <> A.showDefault
         <> A.value 8
         <> A.metavar "INT")
    <*> A.option A.auto
        (A.long "size"
         <> A.short 's'
         <> A.help "Image size (width and height) in pixels"
         <> A.showDefault
         <> A.value 256
         <> A.metavar "INT")
    <*> A.option colorReader
        (A.long "background"
         <> A.short 'b'
         <> A.help "Background color (hex values)"
         <> A.showDefault
         <> A.value white
         <> A.metavar "RRGGBB")
    <*> A.option colorReader
        (A.long "link"
         <> A.short 'l'
         <> A.help "Link color (hex values)"
         <> A.showDefault
         <> A.value lightBlue
         <> A.metavar "RRGGBB")
    <*> A.option A.auto
        (A.long "link-width"
         <> A.short 'w'
         <> A.help "Link width in pixels"
         <> A.showDefault
         <> A.value 6
         <> A.metavar "FLOAT")
    <*> A.switch
        (A.long "trace"
         <> A.help "Draw a trace of the pendulum end")
    <*> A.option colorReader
        (A.long "trace-color"
         <> A.help "Color of the trace (hex values)"
         <> A.showDefault
         <> A.value lightGray
         <> A.metavar "RRGGBB")
    <*> A.option A.auto
        (A.long "trace-width"
         <> A.help "Width of the trace in pixels"
         <> A.showDefault
         <> A.value 1
         <> A.metavar "FLOAT")
    <*> A.optional (A.option A.auto
        (A.long "trace-limit"
         <> A.help "Limit for the trace length (seconds)"
         <> A.showDefault
         <> A.metavar "FLOAT"))
    <*> A.argument A.str (A.metavar "FILE.GIF")
    <*> A.option A.auto
        (A.long "angle0"
         <> A.help "Angle of the top segment (degrees)"
         <> A.showDefault
         <> A.value 90
         <> A.metavar "FLOAT")
    <*> A.option A.auto
        (A.long "angle1"
         <> A.help "Angle of the bottom segment (degrees)"
         <> A.showDefault
         <> A.value 180
         <> A.metavar "FLOAT")


colorReader :: A.ReadM InputColor
colorReader = A.maybeReader pColor


pColor :: String -> Maybe InputColor
pColor [rl, rh, gl, gh, bl, bh] = do
  let
    pNibble :: Char -> Maybe Word8
    pNibble c | c >= '0' && c <= '9' = Just . fromIntegral $ fromEnum c - 48
              | c >= 'A' && c <= 'F' = Just . fromIntegral $ fromEnum c - 65 + 10
              | c >= 'a' && c <= 'f' = Just . fromIntegral $ fromEnum c - 97 + 10
              | otherwise            = Nothing
    pLow = pNibble
    pHigh c = flip shift 4 <$> pNibble c
  InputColor
    <$> ((+) <$> pLow rl <*> pHigh rh)
    <*> ((+) <$> pLow gl <*> pHigh gh)
    <*> ((+) <$> pLow bl <*> pHigh bh)
pColor _ = Nothing


toHex :: Word8 -> String
toHex x =
  let
    loNibble :: Word8 -> Char
    loNibble n | n < 10    = toEnum . fromIntegral $ n + 48
               | otherwise = toEnum . fromIntegral $ n - 10 + 65
  in
    [ loNibble (shift x (-4) .&. 0x0F), loNibble (x .&. 0x0F) ]
