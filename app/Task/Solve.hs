module Task.Solve where

import Data.Array (listArray, (!))

import Times
import CauchyProblem
import Task (g, m, x0, y0)
import NumericalMethods
import NumericalMethods.Methods

{-
    Function that applies numerical method to the task.
-}
solve :: NumericalMethod -> Timegrid -> (VarValue, VarValue) -> Solution
solve method timegrid (x'0, y'0) = let r3 vars = sqrt (vars!'a' ** 2.0 + vars!'c' ** 2.0) ** 3.0
                                       -- 'a' is x and 'c' is y and 'b' is dx/dt and 'd' is dy/dt
                                       x_eq _ vars = vars!'b'
                                       y_eq _ vars = vars!'d'
                                       x'_eq _ vars = (-g) * m / r3 vars * vars!'a'
                                       y'_eq _ vars = (-g) * m / r3 vars * vars!'c'
                                       problem = listArray ('a', 'd') [(x0, x_eq), (x'0, x'_eq), (y0, y_eq), (y'0, y'_eq)]
                                    in method timegrid problem

{-
    Solves the task by using Euler's explicit method.
-}
solveExplicit :: Timegrid -> (VarValue, VarValue) -> Solution
solveExplicit = solve methodExplicit

{-
    Solves the task by using trapezoid method.
-}
solveTrapezoid :: Timegrid -> (VarValue, VarValue) -> Solution
solveTrapezoid = solve methodTrapezoid

{-
    Solves the task by using Euler's implicit method.
-}
solveImplicit :: Timegrid -> (VarValue, VarValue) -> Solution
solveImplicit = solve methodImplicit