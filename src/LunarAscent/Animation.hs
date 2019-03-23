{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module LunarAscent.Animation where

import           Control.Lens                ((^.))
import qualified Data.Colour.SRGB            as SRGB
import qualified Diagrams.Backend.Rasterific as BR
import qualified Diagrams.Backend.SVG        as SVG
import           Diagrams.Prelude            (Any, Path, QDiagram, Renderable,
                                              SizeSpec, TypeableFloat, V2,
                                              ( # ), (^&), _x, _y)
import qualified Diagrams.Prelude            as D


test :: IO ()
test = do
  let
    size = D.dims2D 3840 2160
    dia = D.circle (10 :: Double) # D.translate (D.V2 40 20)
    padded = dia # D.centerXY # D.pad 1.05
    gradBG = bgGrad size padded

  SVG.renderSVG "test.svg" size gradBG


-- | Apply a nice background gradient to a diagram.
--
-- This is somewhat involved since we need to figure out the size of
-- the ultimate image, not just the bounding rect of the diagram,
-- which is why the logic from 'D.bg' won't work directly.
bgGrad
  :: ( TypeableFloat n, Renderable (Path V2 n) b )
  => SizeSpec V2 n         -- size of the ultimate image
  -> QDiagram b V2 n Any
  -> QDiagram b V2 n Any
bgGrad sizeSpec d =
  let
    -- figure out how to fit the diagram into the sizeSpec...
    --   (wd, hd) are the final width and height of the diagram;
    --   xform is the transformation to make it fit
    (D.V2 wd hd, xform) = D.sizeAdjustment sizeSpec (D.boundingBox d)

    -- finalW and finalH are the final width and height
    finalW = maybe wd id ((D.getSpec sizeSpec)^._x)
    finalH = maybe hd id ((D.getSpec sizeSpec)^._y)

    -- gradient
    topColour    = SRGB.sRGB24read "#06263E"
    bottomColour = SRGB.sRGB24read "#060B1C"
    gradient = D.mkLinearGradient
               (D.mkStops
                [ (topColour,    0, 1)
                , (bottomColour, 1, 1) ])
               (0 ^& finalH)
               (0 ^& 0)
               D.GradPad
  in
    D.transform xform d
    <> ( D.rect finalW finalH
       # D.translate (D.V2 (finalW/2) (finalH/2))
       # D.fillTexture gradient
       # D.lw 0 )
