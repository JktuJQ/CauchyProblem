{-
 `Plotting` module allows plotting functions and data arrays.
-}
module Plotting where

import Graphics.EasyPlot

import CauchyProblem
import Times

{-
 `PlotStyle` lists possible stylings of plotted data.
-}
data PlotStyle = WithLines | WithPoints | WithCrosses | WithBars | WithLinesAndPoints

{-
 `Plot2D` type is an alias for `Graph2D Float Float` from `easyplot` package.
-}
type Plot2D = Graph2D Float Float

{-
 `Fn2D` type represents `f(t) = ?` equation.
-}
type Fn2D = Float -> Float
{-
 `plotFn2D` function plots `Fn2D`.
-}
plotFn2D :: String -> (Float, Float) -> Fn2D -> Plot2D
plotFn2D name (x_start, x_end) = Function2D [Title name] [Range x_start x_end]

{-
 `plotData2D` function plots set of points.
-}
plotData2D :: String -> Maybe PlotStyle -> [(Time, VarValue)] -> Plot2D
plotData2D name style = Data2D [Title name, Style $ convertStyles style] []
 where
    convertStyles :: Maybe PlotStyle -> Style
    convertStyles plot_style = case plot_style of
                                   Just WithLines            -> Lines
                                   Just WithPoints           -> Dots
                                   Just WithCrosses          -> Points
                                   Just WithBars             -> Impulses
                                   Just WithLinesAndPoints   -> Linespoints
                                   Nothing                   -> Dots


{-
 Saves plots into given file.
-}
savePlot2D :: FilePath -> [Plot2D] -> IO Bool
savePlot2D path = plot (PNG path)
