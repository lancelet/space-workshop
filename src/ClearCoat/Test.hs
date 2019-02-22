{-# LANGUAGE OverloadedStrings #-}
module ClearCoat.Test where

import qualified Control.Concurrent.STM.TBQueue as TBQueue
import           Control.Monad                  (mapM_)
import qualified Control.Monad.STM              as STM
import qualified Data.IORef                     as IORef
import           Data.Maybe                     (mapMaybe)
import qualified Graphics.Rendering.OpenGL      as GL
import           Linear                         (V2 (V2))
import           SDL                            (($=))
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

  let app = testWidget
  appState <- IORef.newIORef (initState app)  -- TODO: proper statee
  eventQueue <- STM.atomically $ TBQueue.newTBQueue 100


  -- TODO: a lot of the event processing could be done purely
  
  let queueSDLEvents = do
        sdlEvents <- SDL.pollEvents
        let appEvents = mapMaybe translateSDLEvent sdlEvents
        mapM_ (STM.atomically . (TBQueue.writeTBQueue eventQueue)) appEvents

  let processEvents = do
        hasEvent <- STM.atomically $ TBQueue.isEmptyTBQueue eventQueue
        if hasEvent
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
        GL.clear [GL.ColorBuffer]
        queueSDLEvents
        status <- processEvents
        case status of
          EventLoopQuit -> pure ()
          EventLoopContinue -> do
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
testDraw info = Rectangle (bounds info)


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
  = Rectangle Rect


data Event e
  = EventCustom e
  | EventQuit
