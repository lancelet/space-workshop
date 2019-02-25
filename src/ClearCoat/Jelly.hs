{-# LANGUAGE OverloadedStrings #-}

module ClearCoat.Jelly where

import           Control.Monad             (mapM_)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import qualified Data.ByteString.Char8     as BS
import           Data.Colour               (AlphaColour)
import qualified Data.Colour               as Colour
import qualified Data.Colour.SRGB          as SRGB
import qualified Data.Foldable             as Foldable
import qualified Data.Vector.Storable      as V
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import           Linear                    (V2 (V2), M23, _x, _y, _z)
import qualified Linear                    as Linear
import Control.Lens ((^.))
import Foreign.Ptr (nullPtr)

import qualified ClearCoat.Escher          as Escher
import           ClearCoat.Types           (MitreLimit, Path, PathWidth)


drawPath
  :: ( Foldable t
     , RealFloat a, Linear.Epsilon a, V.Storable a, GL.MatrixComponent a, Show a
     , Real b )
  => JellyGLResources
  -> M23 a
  -> AlphaColour b
  -> MitreLimit a
  -> PathWidth a
  -> Path t a
  -> IO ()
drawPath res projection colour mitreLimit width path =
  let
    mesh = Escher.strokePathMitre mitreLimit width path
  in do
    -- shadeConstantTriMesh res projection (Colour.alphaColourConvert colour) mesh
    shadeConstantTriMesh res projection (Colour.alphaColourConvert colour)
      ( Escher.TriMesh
        [ Escher.Tri (V2 0 1) (V2 (-1) (-1)) (V2 1 (-1)) ]
        -- [ Escher.Tri (V2 0 (0.4)) (V2 (-0.4) (-0.4)) (V2 (0.4) (-0.4)) ]
      )


data JellyGLResources
  = JellyGLResources
    { progConstant :: GL.Program
    }


initJellyGLResources :: IO (Maybe JellyGLResources)
initJellyGLResources = runMaybeT $ do
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  vbo <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  
  vertexShader <- MaybeT $ compileShader GL.VertexShader baseVertexSrc
  constantShader <- MaybeT $ compileShader GL.FragmentShader constantFragSrc

  pConstant <- MaybeT $ linkProgram [ vertexShader, constantShader ]

  pure JellyGLResources
    { progConstant = pConstant }


shadeConstantTriMesh
  :: ( Foldable t
     , V.Storable a, GL.MatrixComponent a, Num a )
  => JellyGLResources
  -> M23 a
  -> AlphaColour Float
  -> Escher.TriMesh t a
  -> IO ()
shadeConstantTriMesh res projection colour mesh = do
  let coord2d = GL.AttribLocation 0
  let vs = triMesh2Vec mesh
  let prog = progConstant res
  let glcolor = alphaColourToColor4 colour
  GL.currentProgram $= (Just prog)
  colorUniform <- GL.uniformLocation prog "surface_color"
  projectionUniform <- GL.uniformLocation prog "projection"
  GL.uniform colorUniform $= glcolor
  mat <- mat23GLMatrix projection
  GL.uniform projectionUniform $= mat

  vertexBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
  V.unsafeWith vs $ \ptr -> do
    let size = fromIntegral (V.length vs)
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
  GL.vertexAttribPointer coord2d $=
    (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 nullPtr)
  {-
  V.unsafeWith vs $ \ptr -> do
    GL.vertexAttribPointer coord2d $=
      (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 ptr)
  -}
  GL.vertexAttribArray coord2d $= GL.Enabled

  GL.validateProgram prog
  validateOK <- GL.get $ GL.validateStatus prog
  if not validateOK then do
    putStrLn "GL.linkProgram error:"
    plog <- GL.get $ GL.programInfoLog prog
    putStrLn plog
    else pure ()
  
  GL.drawArrays GL.Triangles 0 (fromIntegral (V.length vs))
  GL.vertexAttribArray coord2d $= GL.Disabled
  -- GL.bindVertexArrayObject $= Nothing


alphaColourToColor4 :: AlphaColour Float -> GL.Color4 Float
alphaColourToColor4 colour =
  let
    SRGB.RGB r g b = SRGB.toSRGB
                   $ Colour.darken (recip a) (colour `Colour.over` Colour.black)
    a = Colour.alphaChannel colour
  in
    GL.Color4 r g b a


mat23GLMatrix
  :: ( GL.MatrixComponent a, Num a )
  => M23 a
  -> IO (GL.GLmatrix a)
mat23GLMatrix m = do
  let
    r = m^._x
    s = m^._y 
    components =
      [ r^._x, r^._y, 0, r^._z
      , s^._x, s^._y, 0, s^._z
      ,     0,     0, 1,     0
      ,     0,     0, 0,     1 ]
  GL.newMatrix (GL.RowMajor) components
  

triMesh2Vec
  :: ( Foldable t
     , V.Storable a )
  => Escher.TriMesh t a
  -> V.Vector a
triMesh2Vec (Escher.TriMesh tris) =
  let
    stri (Escher.Tri (V2 x1 y1) (V2 x2 y2) (V2 x3 y3))
      = [ x1, y1, x2, y2, x3, y3 ]
  in
    V.fromList . concat . fmap stri . Foldable.toList $ tris


compileShader :: GL.ShaderType -> BS.ByteString -> IO (Maybe GL.Shader)
compileShader typ src = do
  shader <- GL.createShader typ
  GL.shaderSourceBS shader $= src
  GL.compileShader shader
  ok <- GL.get $ GL.compileStatus shader
  if ok
    then pure . Just $ shader
    else do
      putStrLn "Error in shader (source below):"
      putStrLn . BS.unpack $ src
      slog <- GL.get $ GL.shaderInfoLog shader
      putStrLn slog
      pure Nothing


linkProgram :: [GL.Shader] -> IO (Maybe GL.Program)
linkProgram shaders = do
  program <- GL.createProgram
  mapM_ (GL.attachShader program) shaders
  GL.bindFragDataLocation program "FragColor" $= 0
  GL.linkProgram program
  linkOK <- GL.get $ GL.linkStatus program
  -- GL.validateProgram program
  -- validateOK <- GL.get $ GL.validateStatus program
  if (linkOK {- && validateOK -})
    then pure . Just $ program
    else do
      putStrLn "GL.linkProgram error:"
      plog <- GL.get $ GL.programInfoLog program
      putStrLn plog
      pure Nothing


baseVertexSrc :: BS.ByteString
baseVertexSrc = BS.intercalate "\n"
  [ "#version 410 core"  -- 
  , ""
  , "layout(location = 0) in vec2 coord2d;"
  , "uniform mat4 projection;"
  , ""
  , "void main(void) {"
  , "  gl_Position = projection * vec4(coord2d, 0, 1);"
  , "}"
  ]


constantFragSrc :: BS.ByteString
constantFragSrc = BS.intercalate "\n"
  [ "#version 410 core"  --
  , ""
  , "uniform vec4 surface_color;"
  , "out vec4 FragColor;"
  , ""
  , "void main(void) {"
  , "  FragColor = surface_color;"
  , "}"
  ]

