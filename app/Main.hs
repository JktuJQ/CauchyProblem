module Main where

import Data.Array((!))

import Plotting
import CauchyProblem
import Times
import Task
import Task.Explicit

main :: IO ()
main = do
    let range = (0.0, 10.0) :: (Time, Time)
    let ((t0, t1), tau) = (range, 0.0001) :: ((Time, Time), Time)

    _ <- putStrLn "Enter v0_x"
    line_x <- getLine
    _ <- putStrLn "Enter v0_y"
    line_y <- getLine

    let timegrid = createTimegrid (t0, t1) tau :: Timegrid
    let (timeline, result) = solve timegrid (x0, y0) (x'0 line_x, y'0 line_y) :: Solution
    let x_numerical_solution = zip timeline [step!'x' | step <- result] :: [(Time, VarValue)]
    let y_numerical_solution = zip timeline [step!'y' | step <- result] :: [(Time, VarValue)]
    let xy_numerical_solution = filter (\(x, y) -> re <= sqrt (x * x + y * y) && sqrt (x * x + y * y) <= 10.0 ^ 6) [(step!'x', step!'y') | step <- result]

    --print $ show $ take 100 xy_numerical_solution

    let plot_numerical_x = plotData2D "x-numerical-solution" (Just WithPoints) x_numerical_solution :: Plot2D
    let plot_numerical_y = plotData2D "y-numerical-solution" (Just WithPoints) y_numerical_solution :: Plot2D
    let plot_numerical_xy = plotData2D "numerical-solution" (Just WithPoints) xy_numerical_solution :: Plot2D
    
    let plot_earth = plotData2D "Earth" (Just WithPoints) ([(sqrt (re * re - y * y), y) | y <- [-re..re]] ++ [(-sqrt (re * re - y * y), y) | y <- [-re..re]])

    _ <- savePlot2D "plots/x-numerical-solution.png" [plot_numerical_x]
    _ <- savePlot2D "plots/y-numerical-solution.png" [plot_numerical_y]
    _ <- savePlot2D "plots/xy-numerical-solution.png" [plot_numerical_xy, plot_earth]

    putStrLn "Done!"
    _ <- getLine
    print ""
