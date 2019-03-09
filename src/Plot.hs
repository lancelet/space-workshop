{-# LANGUAGE TypeApplications #-}
module Plot where


import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import qualified Graphics.Gnuplot.Advanced             as GP
import qualified Graphics.Gnuplot.Frame                as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet      as Opts
import qualified Graphics.Gnuplot.Graph                as Graph
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification    as LineSpec
import qualified Graphics.Gnuplot.Plot                 as Plot
import qualified Graphics.Gnuplot.Plot.TwoDimensional  as Plot2D
import qualified Graphics.Gnuplot.Terminal.PNG         as PNG
import qualified Graphics.Gnuplot.Terminal.SVG         as SVG
import System.IO (stdout)
import qualified System.IO.Temp                        as Temp
import qualified ITermShow
import qualified Data.ByteString.Lazy as LBS

data Output
  = Screen
  | PNG FilePath
  | SVG FilePath


newtype Title  = Title Text
newtype XLabel = XLabel Text
newtype YLabel = YLabel Text
data Item
  = Line Text [(Double, Double)]
  | Points Text [(Double, Double)]


plotLinesGNUPlot
  :: Output
  -> Title
  -> XLabel
  -> YLabel
  -> [Item]
  -> IO ()
plotLinesGNUPlot out title xlabel ylabel items =
  case out of
    Screen -> do
      tempFilePath <- Temp.emptySystemTempFile "plot-png-"
      plotLinesGNUPlotPNG tempFilePath title xlabel ylabel items
      showPNG tempFilePath
    PNG filePath -> plotLinesGNUPlotPNG filePath title xlabel ylabel items
    SVG filePath -> plotLinesGNUPlotSVG filePath title xlabel ylabel items


showPNG :: FilePath -> IO ()
showPNG filePath
    = LBS.readFile filePath
  >>= LBS.hPutStr stdout . ITermShow.displayImage
   >> putStrLn ""


plotLinesGNUPlotPNG
  :: FilePath
  -> Title
  -> XLabel
  -> YLabel
  -> [Item]
  -> IO ()
plotLinesGNUPlotPNG filePath title xLabel yLabel items =
  GP.plot (PNG.cons filePath)
          (gnuPlotFrame title xLabel yLabel $ gnuPlotItems items)
  >> pure ()


plotLinesGNUPlotSVG
  :: FilePath
  -> Title
  -> XLabel
  -> YLabel
  -> [Item]
  -> IO ()
plotLinesGNUPlotSVG filePath title xLabel yLabel items =
  GP.plot (SVG.cons filePath)
          (gnuPlotFrame title xLabel yLabel $ gnuPlotItems items)
  >> pure ()


gnuPlotFrame :: (Graph.C graph) => Title -> XLabel -> YLabel -> Plot.T graph -> Frame.T graph
gnuPlotFrame (Title title) (XLabel xLabel) (YLabel yLabel)
  = Frame.cons
    $ Opts.title (Text.unpack title)
    $ Opts.xLabel (Text.unpack xLabel)
    $ Opts.yLabel (Text.unpack yLabel)
    $ Opts.gridXTicks True
    $ Opts.gridYTicks True
    $ Opts.size 800 600
    $ Opts.deflt


gnuPlotItems :: [Item] -> Plot2D.T Double Double
gnuPlotItems items = mconcat $ fmap plotItem items
  where
    plotItem :: Item -> Plot2D.T Double Double
    plotItem item = case item of
      Line title values   -> gnuPlotLine title values
      Points title values -> gnuPlotPoints title values


gnuPlotLine :: Text -> [(Double, Double)] -> Plot2D.T Double Double
gnuPlotLine title values = gnuSetTitle title (Plot2D.list Graph2D.lines values)


gnuPlotPoints :: Text -> [(Double, Double)] -> Plot2D.T Double Double
gnuPlotPoints title values = gnuSetTitle title (Plot2D.list Graph2D.points values)


gnuSetTitle :: Functor f => Text -> f (Graph2D.T x y) -> f (Graph2D.T x y)
gnuSetTitle title = fmap (Graph2D.lineSpec (LineSpec.title (Text.unpack title) $ LineSpec.deflt))
