{-
    `Task.Solve` module applies several numerical methods to exact task
    to obtain solution.
-}
module Task.Solve where

import Data.Array (listArray, (!))

import Times
import CauchyProblem
import Task (g, m, x0, y0, distance)
import NumericalMethods
import NumericalMethods.Methods

{-
    Function that applies numerical method to the task.
-}
solve :: NumericalMethod -> Timegrid -> (VarValue, VarValue) -> Solution
solve method timegrid (x'0, y'0) = let r3 vars = distance (vars!'a', vars!'c') ** 3.0
                                       -- 'a' is x and 'c' is y and 'b' is dx/dt and 'd' is dy/dt
                                       x_eq _ vars = vars!'b'
                                       y_eq _ vars = vars!'d'
                                       x'_eq _ vars = (-g) * m / r3 vars * vars!'a'
                                       y'_eq _ vars = (-g) * m / r3 vars * vars!'c'
                                       problem = listArray ('a', 'd')
                                                 [(x0, x_eq), (x'0, x'_eq), (y0, y_eq), (y'0, y'_eq)]
                                    in method timegrid problem

{-
    `SolveMethod` type describes methods that solve exact task.
-}
type SolveMethod = Timegrid -> (VarValue, VarValue) -> Solution

{-
    Solves the task by using Euler's explicit method.
-}
solveExplicit :: SolveMethod
solveExplicit = solve methodExplicit

{-
    Solves the task by using trapezoid method.
-}
solveTrapezoid :: SolveMethod
solveTrapezoid = solve methodTrapezoid

{-
    Solves the task by using Runge-Kutta method.
-}
solveRungeKutta :: SolveMethod
solveRungeKutta = solve methodRungeKutta

