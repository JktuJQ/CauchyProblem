module Main where

import Data.Array(listArray, (!))

import Plotting
import CauchyProblem
import NumericalMethods.ErrorMargin
import NumericalMethods.Euler
import Times

solve :: Timegrid -> Problem -> Solution
solve = methodTrapezoid

main :: IO ()
main = do
    let range = (0.0, 1.0) :: (Time, Time)
    let ((t0, t1), tau) = (range, 0.01) :: ((Time, Time), Time)

    let (x0, y0) = (1.0, 1.0) :: (VarValue, VarValue)
    let original_solution t = (0.5 * exp t) + (0.5 * exp (5 * t))
    let x_eq _ vars = (2.0 * (vars!'x')) + (vars!'y')
    let y_eq _ vars = (3.0 * (vars!'x')) + (4.0 * (vars!'y'))
    
    let timegrid = createTimegrid (t0, t1) tau :: Timegrid
    let problem = listArray ('x', 'y') [(x0, x_eq), (y0, y_eq)] :: Problem
    let (timeline, result) = solve timegrid problem :: Solution
    let x_numerical_solution = zip timeline [step!'x' | step <- result] :: [(Time, VarValue)]

    let plot_original = plotFn2D "original-solution" range original_solution
    let plot_numerical = plotData2D "numerical-solution" (Just WithBars) x_numerical_solution :: Plot2D
    _ <- savePlot2D "plots/x-original-solution.png" [plot_original]
    _ <- savePlot2D "plots/x-numerical-solution.png" [plot_numerical]
    _ <- savePlot2D "plots/x-both-solutions.png" [plot_original, plot_numerical]

    let taus = iterateTau 1 10
    let log_taus = map (negate . log) taus
    let results0 = [methodExplicit (createTimegrid (t0, t1) tau) problem | tau <- taus]
    let results1 = [methodTrapezoid (createTimegrid (t0, t1) tau) problem | tau <- taus]

    let error_check = maxErrorMargin original_solution
    let error_margins0 = zip log_taus [-log (error_check (zip timeline [step!'x' | step <- result])) | (timeline, result) <- results0]
    let error_margins1 = zip log_taus [-log (error_check (zip timeline [step!'x' | step <- result])) | (timeline, result) <- results1]

    let plot_errormargin_explicit = plotData2D "error-margin-explicit" (Just WithLines) error_margins0 :: Plot2D
    let plot_errormargin_trapezoid = plotData2D "error-margin-trapezoid" (Just WithLines) error_margins1 :: Plot2D

    _ <- savePlot2D "plots/x-marginerror.png" [plot_errormargin_explicit, plot_errormargin_trapezoid]

    print ""
