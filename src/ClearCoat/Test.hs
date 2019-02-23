{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ClearCoat.Test where

import qualified Control.Concurrent.STM.TBQueue as TBQueue
import           Control.Lens                   ((^.))
import           Control.Monad                  (mapM_, unless)
import qualified Control.Monad.STM              as STM
import qualified Data.ByteString                as BS
import           Data.Colour                    (Colour)
import qualified Data.Colour.Names              as Colour.Names
import qualified Data.IORef                     as IORef
import           Data.Maybe                     (mapMaybe)
import qualified Data.Vector.Storable           as V
import           Foreign.C.Types                (CInt)
import qualified Graphics.Rendering.OpenGL      as GL
import           Linear                         (V2 (V2), _x, _y)
import           SDL                            (($=))
import qualified SDL
import           System.Exit                    (exitFailure)
import qualified System.IO                      as SysIO

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


  -- Compile shaders and stuff
  shaders <- initPrograms

  let app = testWidget
  appState <- IORef.newIORef (initState app)  -- TODO: proper statee
  eventQueue <- STM.atomically $ TBQueue.newTBQueue 100


  -- TODO: a lot of the event processing could be done purely

  let queueSDLEvents = do
        sdlEvents <- SDL.pollEvents
        let appEvents = mapMaybe translateSDLEvent sdlEvents
        mapM_ (STM.atomically . (TBQueue.writeTBQueue eventQueue)) appEvents

  let processEvents = do
        noEvents <- STM.atomically $ TBQueue.isEmptyTBQueue eventQueue
        if (not noEvents)
          then do
            state <- IORef.readIORef appState
            event <- STM.atomically $ TBQueue.readTBQueue eventQueue
            case (handleEvent app) state event of
              NextQuit -> pure EventLoopQuit
              NextContinue (state', newEvents) -> do
                mapM_ (STM.atomically . (TBQueue.writeTBQueue eventQueue)) newEvents
                IORef.writeIORef appState state'
                processEvents
          else
            pure EventLoopContinue


  let loop = do
        queueSDLEvents
        status <- processEvents
        case status of
          EventLoopQuit -> pure ()
          EventLoopContinue -> do
            state <- IORef.readIORef appState
            -- wsz :: V2 CInt <- SDL.get $ SDL.windowSize window
            wsz :: V2 CInt <- SDL.glGetDrawableSize window
            putStrLn $ "wsz = " ++ show wsz
            let drawInfo = DrawInfo (Rect 0 0 (fromIntegral (wsz^._x)) (fromIntegral (wsz^._y)))
            GL.scissor $= Nothing
            GL.clearColor $= GL.Color4 0 0 0 0
            GL.clear [GL.ColorBuffer]
            GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral (wsz^._x)) (fromIntegral (wsz^._y)))
            GL.matrixMode $= GL.Projection
            GL.loadIdentity
            GL.ortho 0 (fromIntegral (wsz^._x)) (fromIntegral (wsz^._y)) 0 (-1) 1
            GL.matrixMode $= GL.Modelview 0
            epoxyDraw shaders ((draw app) state drawInfo)
            SDL.glSwapWindow window
            loop


  loop

  SDL.destroyWindow window
  SDL.quit


testWidget :: Widget () ()
testWidget
  = Widget
    { draw = const testDraw
    , handleEvent = const testHandleEvent
    , initState = ()
    }


testDraw :: DrawInfo -> Picture
testDraw info
  = Rectangle fill NoStroke (bounds info)
  where
    fill = FillConstant (Colour.Names.blue)


testHandleEvent :: Event e -> Next ((), [Event e])
testHandleEvent EventQuit = NextQuit
testHandleEvent _         = NextContinue ((), [])


translateSDLEvent :: SDL.Event -> Maybe (Event e)
translateSDLEvent (SDL.Event _ SDL.QuitEvent) = Just EventQuit
translateSDLEvent _                           = Nothing

-- | Something like App from Brick
data Widget s e
  = Widget
    { draw        :: s -> DrawInfo -> Picture
    , handleEvent :: s -> Event e -> Next (s, [Event e])
    , initState   :: s
    }


data EventLoopState
  = EventLoopQuit
  | EventLoopContinue


data Next a
  = NextQuit
  | NextContinue a


data EventState = EventState


data DrawInfo
  = DrawInfo
    { bounds :: !Rect
    } deriving (Show)


data Rect
  = Rect
    { x      :: !Float
    , y      :: !Float
    , width  :: !Float
    , height :: !Float
    } deriving (Eq, Show)


data Picture
  = Rectangle !Fill !Stroke !Rect


data Fill
  = NoFill
  | FillConstant !(Colour Double)


data Stroke
  = NoStroke


data Event e
  = EventCustom e
  | EventQuit


epoxyDraw :: Shaders -> Picture -> IO ()
epoxyDraw shaders picture = do
  case picture of
    Rectangle fill stroke rect -> epoxyDrawRectangle shaders fill stroke rect


epoxyDrawRectangle :: Shaders -> Fill -> Stroke -> Rect -> IO ()
epoxyDrawRectangle shaders fill _ rect = do
  GL.currentProgram $= Just (progConstant . programs $ shaders)
  GL.vertexAttribArray (attribCoord2d . attribTable $ shaders) $= GL.Enabled
  let
    Rect rx ry rw rh = rect
    verts :: V.Vector Float
    verts = V.fromList
      [    rx,    ry
      ,    rx, ry+rh
      , rx+rw, ry+rh
      , rx+rw,    ry
      ,    rx,    ry
      , rx+rw, ry+rh
      ]
    color :: GL.Color4 Float
    color = case fill of
      NoFill         -> GL.Color4 0 0 1 1
      FillConstant _ -> GL.Color4 0 1 0 1
  putStrLn $ "verts = " ++ show verts


  V.unsafeWith verts $ \ptr ->
    GL.vertexAttribPointer (attribCoord2d . attribTable $ shaders)
      $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 ptr)
  colorUniform <- GL.uniformLocation (progConstant . programs $ shaders) "surface_color"
  GL.uniform colorUniform $= color
  GL.drawArrays GL.Triangles 0 6
  GL.vertexAttribArray (attribCoord2d . attribTable $ shaders) $= GL.Disabled


data Shaders
  = Shaders
    { programs    :: Programs
    , attribTable :: AttribTable
    }

data Programs
  = Programs
    { progConstant :: GL.Program
    }

data AttribTable
  = AttribTable
    { attribCoord2d :: GL.AttribLocation }


initPrograms :: IO Shaders
initPrograms = do

  -- compile vertex shader
  vertexShader <- GL.createShader GL.VertexShader
  GL.shaderSourceBS vertexShader $= vertexShaderSrc
  GL.compileShader vertexShader
  vsOK <- GL.get $ GL.compileStatus vertexShader
  unless vsOK $ do
    SysIO.hPutStrLn SysIO.stderr "Error in vertex shader"
    exitFailure

  -- compile fragment shader
  fragShader <- GL.createShader GL.FragmentShader
  GL.shaderSourceBS fragShader $= constantFragShaderSrc
  GL.compileShader fragShader
  fsOK <- GL.get $ GL.compileStatus fragShader
  unless fsOK $ do
    SysIO.hPutStrLn SysIO.stderr "Error in fragment shader"

  -- create a program
  program <- GL.createProgram
  GL.attachShader program vertexShader
  GL.attachShader program fragShader
  GL.attribLocation program "coord2d" $= GL.AttribLocation 0
  GL.linkProgram program
  linkOK <- GL.get $ GL.linkStatus program
  GL.validateProgram program
  status <- GL.get $ GL.validateStatus program
  unless (linkOK && status) $ do
    SysIO.hPutStrLn SysIO.stderr "GL.linkProgram error"
    plog <- GL.get $ GL.programInfoLog program
    putStrLn plog
    exitFailure

  return
    Shaders
    { programs =
        Programs
        { progConstant = program }
    , attribTable =
        AttribTable
        { attribCoord2d = GL.AttribLocation 0 }
    }


vertexShaderSrc :: BS.ByteString
vertexShaderSrc = BS.intercalate "\n"
  [ "attribute vec2 coord2d;"
  , ""
  , "void main(void) {"
  , "  gl_Position = gl_ModelViewProjectionMatrix * vec4(coord2d, 0.0, 1.0);"
  , "}"
  ]


constantFragShaderSrc :: BS.ByteString
constantFragShaderSrc = BS.intercalate "\n"
  [ "uniform vec4 surface_color;"
  , ""
  , "void main(void) {"
  -- , "  gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);"
  , "  gl_FragColor = surface_color;"
  , "}"
  ]
