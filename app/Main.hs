module Main where

import Data.Array(listArray, (!))

import Plotting
import NumericalMethods.Euler
import Times

main :: IO ()
main = do
    let range = (0.0, 1.0)
    let plot_original = let original_solution t = (2.0 * exp t) + (3.0 * exp (5 * t))
                        in plotFn2D "original-solution" range original_solution
    _ <- savePlot2D "plots/x-original-solution.png" [plot_original]
    
    let plot_numerical = let ((t0, t1), tau) = (range, 0.01)
                             (x0, y0) = (5.0, 7.0)
                             x_eq _ vars = (2.0 * (vars!'x')) + (vars!'y')
                             y_eq _ vars = (3.0 * (vars!'x')) + (4.0 * (vars!'y'))
                             conditions = [(x0, x_eq), (y0, y_eq)]
                             problem = listArray ('x', 'y') conditions

                             timegrid = createTimegrid (t0, t1) tau

                             (timeline, result) = methodImplicit timegrid problem

                             x_numerical_solution = zip timeline [step!'x' | step <- result]
                         in plotData2D "numerical-solution" (Just WithBars) x_numerical_solution
    _ <- savePlot2D "plots/x-numerical-solution.png" [plot_numerical]
    
    _ <- savePlot2D "plots/x-both-solutions.png" [plot_original, plot_numerical]
    print ""
