module Main where

import Data.Array((!))

import Graphics.EasyPlot

import CauchyProblem
import Times
import Task
import Task.Solve

main :: IO ()
main = do
    -------- Task data --------
    let range = (0.0, 10000.0) :: (Time, Time)
    let ((t0, t1), tau) = (range, 0.1) :: ((Time, Time), Time)

    _ <- putStrLn "Enter v0_x"
    line_x <- getLine
    _ <- putStrLn "Enter v0_y"
    line_y <- getLine
    ---------------------------

    let (_, result) = solveImplicit (createTimegrid (t0, t1) tau) (x'0 line_x, y'0 line_y) :: Solution

    let xy_numerical_solution = [(step!'a', step!'c') |
                                                      step <- takeWhile (\vars ->
                                                      re <= sqrt (vars!'a' ** 2.0 + vars!'c' ** 2.0)) result]
    let plot_numerical_xy = Data2D [Title "Trajectory", Style Dots, Color Red] [] xy_numerical_solution

    let plot_earth = Data2D [Title "Earth", Style Dots, Color Green] [] ([(sqrt (re * re - y * y), y) | y <- [-re..re]] ++ [(-sqrt (re * re - y * y), y) | y <- [-re..re]])

    _ <- plot (PNG "plots/xy-numerical-solution.png") [plot_numerical_xy, plot_earth]

    putStrLn "Done!"
    _ <- getLine
    print ""
