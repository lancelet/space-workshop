{-# LANGUAGE OverloadedStrings #-}
module ClearCoat.Test where

import           Control.Monad             (unless)
import           Data.Map.Strict           (Map)
import           Data.Text                 (Text)
import qualified Graphics.Rendering.OpenGL as GL
import           Linear                    (V2 (V2))
import           SDL                       (($=))
import qualified SDL

test :: IO ()
test = do
  putStrLn "Hello World"

  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear

  window <- SDL.createWindow
              "ClearCoat Test / Dev"
              SDL.defaultWindow
              { SDL.windowInitialSize = V2 800 600
              , SDL.windowHighDPI = True
              , SDL.windowOpenGL = Just SDL.defaultOpenGL
              , SDL.windowResizable = True
              }
  SDL.showWindow window
  _ <- SDL.glCreateContext window

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        GL.clear [GL.ColorBuffer]
        SDL.glSwapWindow window

        unless quit loop
  loop

  SDL.destroyWindow window
  SDL.quit


-- | Something like App from Brick
data App s e
  = App
    { draw        :: s -> DrawInfo -> Picture
    , handleEvent :: s -> Event e -> Maybe (s, [Event e])
    , styleMap    :: s -> StyleMap
    }

data DrawInfo
  = DrawInfo
    { bounds :: !Rect
    } deriving (Show)

data StyleName = StyleName [Text]

data Attr = Attr

newtype StyleMap = StyleMap (Map StyleName Attr)


data Rect
  = Rect
    { x      :: !Float
    , y      :: !Float
    , width  :: !Float
    , height :: !Float
    } deriving (Eq, Show)


data Picture
  = Rectangle Rect


data Event e
  = CustomEvent e
