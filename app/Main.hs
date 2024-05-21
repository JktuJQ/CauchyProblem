module Main where

import System.Environment
import Data.Array(listArray, (!))

import Graphics.EasyPlot

import Times
import CauchyProblem
import NumericalMethods
import NumericalMethods.Methods
import NumericalMethods.ErrorMargin
import Task
import Task.Solve
import Task.Features

{-
    Tests numerical methods for their errors using exact problem.
-}
errorTests :: [String] -> [NumericalMethod] -> IO ()
errorTests args methods = do
    let x_eq _ vars = 2.0 * (vars!'x') + (vars!'y')
    let y_eq _ vars = 3.0 * (vars!'x') + 4.0 * (vars!'y')
    let problem = listArray ('x', 'y') [(1.0, x_eq), (1.0, y_eq)]

    let original_fn t = (0.0 - 0.5 * exp t) + 1.5 * exp (5.0 * t)
    
    let tests = testMethods ((0.0, 1.0), 0.5, 2.0, 15) problem original_fn methods  
    let methods_errors = [Data2D [Title $ ["Explicit", "Trapezoid", "Runge-Kutta"]!!i, Style Lines] [] [tau_error!!i | tau_error <- tests] | i <- [0..length methods - 1]]
    _ <- (if length args == 1 && head args == "X11"
          then plot' [Interactive] X11
          else plot (PNG (if length args == 2 && head args == "PNG" then last args else "plots/error_test.png"))) methods_errors
    return ()

{-
    Solves for x and y.
-}
xyNumericalSolution :: Timegrid -> SolveMethod -> Double -> Double -> [(VarValue, VarValue)]
xyNumericalSolution timegrid method v0_x v0_y = [(step!'a', step!'c') | step <- result]
 where
    result = stopAtFall (snd $ method timegrid (v0_x, v0_y))

main :: IO ()
main = do
    args <- getArgs -- X11 or PNG filename

    {-
    let check = performChecks (createTimegrid (0.0, 86400.0) 1.0) solveRungeKutta
    let check_fall = check stopAtFlying
    print $ check_fall ((1.0, 5.0, 0.1), (-pi, pi, 0.1))
    print $ check_fall ((1.0, 5.0, 0.1), ((-pi) / 2.0, pi / 2.0, 0.1))
    print $ check_fall ((1.0, 5.0, 0.1), ((-pi) / 3.0, pi / 3.0, 0.1))
    print $ check_fall ((1.0, 5.0, 0.1), ((-pi) / 6.0, pi / 6.0, 0.1))
    print $ check_fall ((1.0, 5.0, 0.1), (0.0, 1.0, 2.0))
    -}

    -------- Task data --------
    _ <- putStrLn "Enter t1 in seconds - default is 24 hours (t0 = 0, t1 - t0 -> simulation's timespan):"
    t_end <- getLine
    _ <- putStrLn "Enter tau in seconds - default is 1 second (precision, simulation's time step):"
    step <- getLine

    _ <- putStrLn "Enter v0_x in km/s - default is 0 km/s:"
    v_x <- getLine
    _ <- putStrLn "Enter v0_y in km/s - default is 0 km/s:"
    v_y <- getLine
    let v0_x = x'0 v_x
    let v0_y = y'0 v_y

    _ <- putStrLn "Choose method which will be used - 1 for Euler's explicit, 2 for trapezoid (default), 3 for Runge-Kutta:";
    index <- getLine
    let methods = [solveExplicit, solveTrapezoid, solveRungeKutta]
    let method = methods!!if index == "" then 1 else read index - 1

    let range = (0.0, if t_end == "" then 86400.0 else read t_end) :: (Time, Time)
    let ((t0, t1), h) = (range, if step == "" then 1.0 else read step) :: ((Time, Time), Time)
    let timegrid = createTimegrid (t0, t1) h :: Timegrid
    ---------------------------

    _ <- errorTests args [methodExplicit, methodTrapezoid, methodRungeKutta]
    
    let earth = [(sqrt (re * re - y * y), y) | y <- [-re..re]] ++
                 [(-sqrt (re * re - y * y), y) | y <- [-re..re]]
    let plot_earth = Data2D [Title "Earth", Style Dots, Color Green] [] earth
    
    let solution = xyNumericalSolution timegrid method v0_x v0_y
    let plot_trajectory = Data2D [Title "Trajectory", Style Dots, Color Red] [] solution

    {-
    let solution1 = xyNumericalSolution timegrid solveExplicit v0_x v0_y
    let plot_trajectory1 = Data2D [Title "Explicit", Style Dots, Color Red] [] solution1

    let solution2 = xyNumericalSolution timegrid solveTrapezoid v0_x v0_y
    let plot_trajectory2 = Data2D [Title "Trapezoid", Style Dots, Color Green] [] solution2

    let solution3 = xyNumericalSolution timegrid solveRungeKutta v0_x v0_y
    let plot_trajectory3 = Data2D [Title "RungeKutta", Style Dots, Color Blue] [] solution3
    -}

    _ <- (if length args == 1 && head args == "X11"
          then plot' [Interactive] X11
          else plot (PNG (if length args == 2 && head args == "PNG" then last args else "plots/simulation.png")))
        [plot_trajectory, plot_earth]
        -- [plot_trajectory1, plot_trajectory2, plot_trajectory3, plot_earth]
    putStrLn ""
    putStrLn "Done!"
    _ <- getLine
    return ()
