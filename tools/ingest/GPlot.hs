module GPlot where

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

gplot :: String -> String -> String -> String -> [(Int,Double)] -> IO ()
gplot title lineTitle yLabel xLabel points = ignore $ Plot.plot X11.cons plot2d
    where
        plot2d = Frame.cons frameSpec $ fmap lineSpec $ Plot2D.list Graph2D.points points
        lineSpec = Graph2D.lineSpec $ LineSpec.title lineTitle $ LineSpec.lineWidth 2.5 $ LineSpec.deflt
        frameSpec = Opts.xLabel xLabel $ Opts.yLabel yLabel $ Opts.title title $ Opts.deflt
        ignore a = a >> return ()
        


main :: IO ()
main =
    do
    let plotSpec = gplot "Title" "lineTitle" "yLabel" "xLabel"
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
