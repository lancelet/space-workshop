{-# LANGUAGE OverloadedStrings #-}
module ClearCoat.Test where

import Control.Monad (unless)
import SDL(($=))
import Linear (V2(V2))
import qualified SDL
import qualified Graphics.Rendering.OpenGL as GL

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
