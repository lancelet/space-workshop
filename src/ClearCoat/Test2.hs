{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ClearCoat.Test2 where

import           Control.Lens              ((^.))
import           Control.Monad             (unless)
import qualified Data.Colour               as Colour
import qualified Data.Colour.Names         as Colour.Names
import qualified Data.Colour.SRGB          as SRGB
import qualified Data.Vector.Storable      as V
import           Foreign.C.Types           (CInt)
import           Foreign.Ptr               (nullPtr)
import           Foreign.Storable          (sizeOf)
import qualified Graphics.Rendering.OpenGL as GL
import           Linear                    (V2 (V2), V3 (V3), _x, _y)
import           SDL                       (($=))
import qualified SDL
import           System.Exit               (exitFailure)

import qualified ClearCoat.Jelly           as Jelly
import qualified ClearCoat.Types           as CCT

-- Best Haskell tutorial I've found:
--  https://github.com/ericnething/opengl-sdl-tutorial/tree/master/3-Rendering-with-OpenGL

test :: IO ()
test = do

  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear

  window <-
    SDL.createWindow
      "ClearCoat"
      SDL.defaultWindow
      { SDL.windowInitialSize = V2 800 600
      , SDL.windowHighDPI = True
      , SDL.windowOpenGL = Just $ SDL.defaultOpenGL
                           { SDL.glProfile = SDL.Core SDL.Normal 4 1 }
      , SDL.windowResizable = True
      }
  SDL.showWindow window
  _ <- SDL.glCreateContext window

  -- init resources
  maybeJellyGLResources <- Jelly.initJellyGLResources
  jellyGLResources <- case maybeJellyGLResources of
    Just resources -> pure resources
    Nothing        -> exitFailure

  let
    loop = do
      sdlEvents <- SDL.pollEvents
      let quit = elem SDL.QuitEvent $ map SDL.eventPayload sdlEvents

      -- draw code
      tempDraw window jellyGLResources

      unless quit loop
  loop



tempDraw :: SDL.Window -> Jelly.JellyGLResources -> IO ()
tempDraw window res = do

  winSize :: V2 CInt <- SDL.glGetDrawableSize window
  let glWinSize = fmap fromIntegral winSize
  let dWinSize = fmap fromIntegral winSize

  GL.clearColor $= GL.Color4 0.03 0.09 0.17 0
  GL.clear [GL.ColorBuffer]

  GL.viewport $= ( GL.Position 0 0
                 , GL.Size (glWinSize^._x) (glWinSize^._y) )

  let
    vertices :: V.Vector (GL.Vertex2 GL.GLfloat)
    vertices = V.fromList
      [ GL.Vertex2     0    0.5
      , GL.Vertex2 (-0.5) (-0.5)
      , GL.Vertex2   0.5  (-0.5)
      ]
    size = fromIntegral $ (V.length vertices) * sizeOf (V.head vertices)

  let prog = Jelly.progConstant res
  GL.currentProgram $= Just prog

  -- uniforms
  uColor <- GL.uniformLocation prog "surface_color"
  uProjection <- GL.uniformLocation prog "projection"
  GL.uniform uColor $= (GL.Color4 0 0 1 1 :: GL.Color4 Float)
  mat :: GL.GLmatrix Float <- Jelly.mat23GLMatrix $
           (V2 (V3 1 0 0)
               (V3 0 1 0) :: V2 (V3 Float))
  GL.uniform uProjection $= mat

  -- set up vertex data
  V.unsafeWith vertices $ \ptr ->
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
  GL.vertexAttribPointer (GL.AttribLocation 0) $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor
        2        -- dimensions per vertex
        GL.Float -- type
        0        -- stride
        nullPtr  -- offset
    )
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  -- GL.drawArrays GL.Triangles 0 3  -- second index is number of vertices
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled

  let orange = SRGB.sRGB 0.64 0.26 0.15

  Jelly.drawPath
    res
    (V2 (V3 (2.0/dWinSize^._x)                  0  (-1))
        (V3                 0  (-2.0/dWinSize^._y)   1 ))
     -- 0.64 0.26 0.15 : Orange color
    (Colour.withOpacity orange 1)
    (CCT.MitreLimit 25)
    (CCT.PathWidth 50)
    (CCT.Path
       [ V2 200.0 200.0
       , V2 800.0 800.0
       , V2 1400.0 200.0
       ])

  Jelly.drawPath
    res
    (V2 (V3 (2.0/dWinSize^._x)                  0  (-1))
        (V3                 0  (-2.0/dWinSize^._y)   1 ))
     -- 0.64 0.26 0.15 : Orange color
    (Colour.withOpacity orange 1)
    (CCT.MitreLimit 25)
    (CCT.PathWidth 50)
    (CCT.Path
       [ V2 600.0 200.0
       , V2 800.0 600.0
       , V2 1000.0 200.0
       ])

  SDL.glSwapWindow window
