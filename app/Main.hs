module Main where


import System.Environment
import Data.Array((!))

import Graphics.EasyPlot

import Times 
import Task
import Task.Solve

main :: IO ()
main = do
    args <- getArgs -- X11 or PNG filename

    -------- Task data --------
    _ <- putStrLn "Enter t1 in seconds - default is 24 * 60 * 60 (t0 = 0, t1 - t0 -> simulation's timespan):"
    t_end <- getLine
    _ <- putStrLn "Enter tau in seconds - default is 1 (precision, simulation's time step):"
    tau <- getLine

    _ <- putStrLn "Enter v0_x in km/s - default is 0:"
    line_x <- getLine
    _ <- putStrLn "Enter v0_y in km/s - default is 0:"
    line_y <- getLine

    _ <- putStrLn "Choose method which will be used - 1 for Euler's explicit (default), 2 for trapezoid, 3 for implicit:";
    index <- getLine
    let methods = [solveExplicit, solveTrapezoid, solveImplicit]
    let method = methods!!if index == "" then 1 else read index - 1

    let range = (0.0, if t_end == "" then 86400.0 else read t_end) :: (Time, Time)
    let ((t0, t1), t) = (range, if tau == "" then 1.0 else read tau) :: ((Time, Time), Time)
    ---------------------------

    let solution = [(step!'a', step!'c') |
                    step <- takeWhile (\vars ->
                    re <= sqrt (vars!'a' ** 2.0 + vars!'c' ** 2.0))
                    (snd $ method (createTimegrid (t0, t1) t) (x'0 line_x, y'0 line_y))]
    let plot_numerical_xy = Data2D [Title "Trajectory", Style Dots, Color Red] [] solution

    let plot_earth = Data2D [Title "Earth", Style Dots, Color Green] [] ([(sqrt (re * re - y * y), y) | y <- [-re..re]] ++ [(-sqrt (re * re - y * y), y) | y <- [-re..re]])

    _ <- plot (if length args == 1 && head args == "X11" then X11 else PNG (if length args == 2 && head args == "PNG" then last args else "plots/simulation.png"))
        [plot_numerical_xy, plot_earth]
    
    putStrLn ""
    putStrLn "Done!"
    _ <- getLine
    return ()
