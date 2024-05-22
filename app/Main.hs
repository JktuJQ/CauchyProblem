module Main where

import System.Environment
import Data.Maybe(fromMaybe)
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
errorTests :: [NumericalMethod] -> [Graph2D Double Double]
errorTests methods = [Data2D [Title $ ["Explicit", "Trapezoid", "Runge-Kutta"]!!i, Style Lines] [] [tau_error!!i | tau_error <- tests]
                      | i <- [0..length methods - 1]]
 where
    x_eq _ vars = 2.0 * (vars!'x') + (vars!'y')
    y_eq _ vars = 3.0 * (vars!'x') + 4.0 * (vars!'y')
    problem = listArray ('x', 'y') [(1.0, x_eq), (1.0, y_eq)]

    original_fn t = (0.0 - 0.5 * exp t) + 1.5 * exp (5.0 * t)
    
    tests = testMethods ((0.0, 1.0), 0.5, 2.0, 15) problem original_fn methods

{-
    Checks for characteric speeds.
-}
calculateSpeed :: ([Vars] -> [Vars]) -> (Vars -> Bool) -> (Double, Double) -> (Double, Double) -> [Graph2D Double Double]
calculateSpeed filterer checker (v_min, v_max) (phi_min, phi_max) = numericalSolution ("v_0 = (" ++ take 4 (show v0_x) ++ ", " ++ take 4 (show v0_y) ++ ")") solveRungeKutta timegrid (x'0 $ show v0_x, y'0 $ show v0_y)
 where
    timegrid = createTimegrid (0.0, 86400.0) 50.0
    check_trajectory = performChecks $ check timegrid solveRungeKutta filterer checker
    (v0_x, v0_y) = fromMaybe (0.0, 0.0) (check_trajectory ((v_min, v_max, 0.1), (phi_min, phi_max, 0.1)))

{-
    Solves for x and y.
-}
numericalSolution :: String -> SolveMethod -> Timegrid -> (Double, Double) -> [Graph2D Double Double]
numericalSolution name method timegrid (v0_x, v0_y) = [Data2D [Title name, Style Lines, Color Red] [] result,
                                                       Data2D [Title "Earth", Style Lines, Color Green] [] earth]
 where
    solution = stopAtFall (snd $ method timegrid (v0_x, v0_y))
    result = [(step!'a', step!'c') | step <- solution]

    earth = [(re * cos phi, re * sin phi) | phi <- [-pi,-pi+0.05..pi]]


{-
    Plots using command line args.
-}
plotAll :: [String] -> [Graph2D Double Double] -> IO Bool
plotAll args = if length args == 1 && head args == "X11"
           then plot' [Interactive] X11
           else plot (PNG (if length args == 2 && head args == "PNG" then last args else "plots/plot.png"))

main :: IO ()
main = do
    args <- getArgs -- X11 or PNG filename

    _ <- putStrLn "Print 'trajectory' to plot a trajectory of satellite (default);"
    _ <- putStrLn "Print 'test' to plot error margins of all methods in a negative logarithmic scale;"
    _ <- putStrLn "Print 'fall' to calculate speed for angle that leads to satellite fall;"
    _ <- putStrLn "Print 'fly' to calculate speed for angle that leads to satellite flying out;"
    choice <- getLine

    plots <- case choice of
                "test" -> return $ errorTests [methodExplicit, methodTrapezoid, methodRungeKutta]
                "fall" -> do
                  (speeds, angles) <- getDataForSpeeds
                  return $ calculateSpeed id (\vars -> re >= distance (vars!'a', vars!'c')) speeds angles
                "fly"  -> do
                  _ <- putStrLn "Which distance from Earth is considered as a threshold (default is 10^5 km)?"
                  d <- getLine
                  (speeds, angles) <- getDataForSpeeds
                  return $ calculateSpeed stopAtFall (\vars -> distance (vars!'a', vars!'c') >= if d == "" then 10.0 ** 5.0 else read d) speeds angles
                _      -> do
                  (method, timegrid, speeds) <- getDataForTrajectory
                  return $ numericalSolution "Trajectory" method timegrid speeds
    
    _ <- plotAll args plots

    putStrLn ""
    putStrLn "Done!"
    _ <- getLine
    
    main
 where
    getDataForTrajectory :: IO (SolveMethod, Timegrid, (Double, Double))
    getDataForTrajectory = do
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
    
        let range = (0.0, if t_end == "" then 86400.0 else read t_end)
        let ((t0, t1), h) = (range, if step == "" then 1.0 else read step)
        let timegrid = createTimegrid (t0, t1) h
    
        return (method, timegrid, (v0_x, v0_y))
    
    getDataForSpeeds :: IO ((Double, Double), (Double, Double))
    getDataForSpeeds = do
        _ <- putStrLn "Enter minimal velocity module - default is 0 km/s:"
        v_min <- getLine
        _ <- putStrLn "Enter maximal velocity module - default is 5 km/s:"
        v_max <- getLine
    
        _ <- putStrLn "Enter minimal angle - default is -180 degrees:"
        angle_min <- getLine
        _ <- putStrLn "Enter maximal angle - default is 180 degrees:"
        angle_max <- getLine
    
        return ((if v_min == "" then 0.0 else read v_min, if v_max == "" then 5.0 else read v_max),
                (if angle_min == "" then -pi else read angle_min * (pi / 180.0), if angle_max == "" then pi else read angle_max * (pi / 180.0)))