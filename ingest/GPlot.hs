module GPlot where

import Control.Monad(void)
import Control.Arrow(second)
import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom as Atom
import System.Exit (ExitCode)

plotter :: (Atom.C x, Atom.C y) => Frame.T (Graph2D.T x y) -> IO ExitCode
plotter = Plot.plot X11.cons

lineSpecDefault  = LineSpec.lineWidth 2.5 LineSpec.deflt

lineSpec title = Graph2D.lineSpec $ LineSpec.title title lineSpecDefault
frameSpec title xLabel yLabel = Opts.title title $ Opts.xLabel xLabel $ Opts.yLabel yLabel Opts.deflt

gplotDouble :: String -> String -> String -> String -> [(Double,Double)] -> IO ()
gplotDouble = gplot

gplot :: (Tuple.C x, Tuple.C y, Atom.C x, Atom.C y) => String -> String -> String -> String -> [(x,y)] -> IO ()
gplot title lineTitle xLabel yLabel points = gplotN title yLabel xLabel [ (lineTitle,points) ]

gplotN :: (Tuple.C x, Tuple.C y, Atom.C x, Atom.C y) => String -> String -> String -> [(String,[(x,y)])] -> IO ()
gplotN title xLabel yLabel graphs = void $ plotter plot2d
    where
        plots = mconcat $ map (\(s,px) -> lineSpec s  <$> Plot2D.list Graph2D.points px ) graphs
        plot2d = Frame.cons ( frameSpec title xLabel yLabel ) plots

renderCurve :: (Atom.C x, Atom.C y) => String -> String -> String -> Plot2D.T x y -> IO ()
renderCurve title xLabel yLabel curve = void $ plotter $ Frame.cons ( frameSpec title xLabel yLabel ) curve 

renderCurves :: (Atom.C x, Atom.C y) => String -> String -> String -> [ Plot2D.T x y ] -> IO ()
--renderCurves t x y = renderCurve t x y . mconcat 
renderCurves t x y px = do
    putStrLn $ show (length px) ++ " plots"
    renderCurves' t x y px
    putStrLn "done"

renderCurves' :: (Atom.C x, Atom.C y) => String -> String -> String -> [ Plot2D.T x y ] -> IO ()
renderCurves' t x y = renderCurve t x y . mconcat 

makeCurve :: (Atom.C x, Atom.C y, Tuple.C x, Tuple.C y) => String -> [(x, y)] -> Plot2D.T x y
makeCurve lineTitle points = lineSpec lineTitle <$> Plot2D.list Graph2D.points points

main :: IO ()
main =
    do
    let title = "Title" ; lineTitle = "lineTitle" ; yLabel =  "yLabel" ; xLabel = "xLabel"
        plotSpec = gplot title lineTitle yLabel xLabel
        rawData' = map (second (2 *)) rawData
        rawData :: [( Int , Double )]
        rawData = [ ( 1 , 4.7431578947368424e-4 )
                  , ( 2 , 4.749947368421053e-3 )
                  , ( 3 , 2.908789473684211e-3 )
                  , ( 4 , 2.922157894736842e-3 )
                  , ( 5 , 3.0599999999999994e-3 )
                  , ( 6 , 9.532631578947369e-4 )
                  , ( 7 , 1.1267894736842103e-3 )
                  , ( 8 , 1.2812631578947367e-3 )
                  , ( 9 , 1.3581052631578944e-3 )
                  , ( 10 , 1.4582631578947366e-3 )
                  , ( 10 , 1.4901578947368419e-3 )
                  , ( 20 , 2.3441052631578943e-3 )
                  , ( 30 , 5.546578947368421e-3 )
                  , ( 40 , 4.257526315789473e-3 )
                  , ( 50 , 5.3823684210526305e-3 )
                  , ( 60 , 6.3570526315789464e-3 )
                  , ( 70 , 9.667210526315789e-3 )
                  , ( 80 , 8.456894736842106e-3 )
                  , ( 90 , 9.562315789473683e-3 )
                  , ( 100 , 1.0423473684210526e-2 )
                  , ( 100 , 1.0580105263157894e-2 )
                  , ( 200 , 1.9722947368421048e-2 )
                  , ( 300 , 3.003889473684211e-2 )
                  , ( 400 , 3.510073684210527e-2 )
                  , ( 500 , 4.2675105263157896e-2 )
                  , ( 600 , 5.175505263157896e-2 )
                  , ( 700 , 5.96893157894737e-2 )
                  , ( 800 , 6.409052631578947e-2 )
                  , ( 900 , 7.02376842105263e-2 )
                  , ( 1000 , 7.627789473684207e-2 )
                  , ( 1000 , 7.888889473684209e-2 )
                  , ( 2000 , 0.1323714210526316 )
                  , ( 3000 , 0.17621726315789477 )
                  , ( 4000 , 0.21038484210526312 )
                  , ( 5000 , 0.2498344736842105 )
                  , ( 6000 , 0.2913087894736842 )
                  , ( 7000 , 0.34046621052631576 )
                  , ( 8000 , 0.38239794736842103 )
                  , ( 9000 , 0.4305454736842106 )
                  , ( 10000 , 0.46570026315789476 )
                  , ( 10000 , 0.4611160526315789 )
                  , ( 20000 , 1.072336842105263 )
                  , ( 40000 , 2.9866671052631575 )
                  , ( 50000 , 4.195321578947368 )
                  , ( 70000 , 7.756188 )
                  , ( 100000 , 13.330933000000002 )
                  ]
    plotSpec rawData
    gplotN title yLabel xLabel [ ("rawData" , rawData ) , ("rawData'" , rawData' ) ]
    let c1 = makeCurve "raw" rawData
        c2 = makeCurve "cooked" rawData'
    renderCurves "assembled in bits" "bitty x" "bitty y" [ c1 , c2 ]

{- failed attempts to use other display options, e.g. synchronous calls to gnuplot
   the 'plotter = Plot.plotDefault' 'works' but is ugly.....


--plotter = Plot.plot ( X11.noPersist X11.cons )
--plotter = Plot.plotDefault

plotterSync :: (Atom.C x, Atom.C y) => Frame.T (Graph2D.T x y) -> IO ExitCode
plotterSync = Plot.plot ( X11.noPersist X11.cons )
--gplot_ :: (Tuple.C x, Tuple.C y, Atom.C x, Atom.C y) => String -> String -> String -> [(String,[(x,y)])] -> IO ()
gplot_ plot title yLabel xLabel graphs = void $ plot plot2d
    where
        plots = mconcat $ map (\(s,px) -> lineSpec s  <$> ( Plot2D.list Graph2D.points px) ) graphs
        plot2d = Frame.cons ( frameSpec title xLabel yLabel ) plots

k :: (Tuple.C x, Tuple.C y, Atom.C x, Atom.C y) => String -> String -> String -> [(String,[(x,y)])] -> IO ()
k = gplot_ plotter

-}
