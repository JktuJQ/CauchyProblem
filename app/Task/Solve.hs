module Task.Solve where

import Data.Array (listArray, (!))
import Data.Char

import Times
import CauchyProblem
import Task (g, m, x0, y0)
import NumericalMethods.Euler

{-
    Solves exact task.
-}
{-
solve :: Timegrid -> (VarValue, VarValue) -> (VarValue, VarValue) -> Solution
solve (h, timeline) (x0, y0) (x'0, y'0) = 
    let
        calculate :: Time -> (VarValue, VarValue) -> (VarValue, VarValue) -> (VarValue, VarValue)
        calculate h (x_prev, y_prev) (x'_prev, y'_prev) = (x, y)
         where
            x = x_prev + h * x'_prev - h * h * g * m / (sqrt (x_prev * x_prev + y_prev * y_prev) ^ 3) * x_prev
            y = y_prev + h * y'_prev - h * h * g * m / (sqrt (x_prev * x_prev + y_prev * y_prev) ^ 3) * y_prev

        calculate' :: Time -> (VarValue, VarValue) -> (VarValue, VarValue) -> (VarValue, VarValue)
        calculate' h (x_prev, y_prev) (x'_prev, y'_prev) = (x', y')
         where
            x' = x'_prev - (g * m) / (sqrt (x_prev * x_prev + y_prev * y_prev) ^ 3) * x_prev * h
            y' = y'_prev - (g * m) / (sqrt (x_prev * x_prev + y_prev * y_prev) ^ 3) * y_prev * h
        
        step :: (Time, (Float, [Time])) -> (VarValue, VarValue) -> (VarValue, VarValue) -> [Vars]
        step (h, (_, [])) (x0, y0) (x'0, y'0) = [array ('a','c') [('x', x), ('y', y)]]
         where
            (x, y) = calculate h (x0, y0) (x'0, y'0)
        step (h, (i, timeline)) (x0, y0) (x'0, y'0) = array ('a','c') [('x', x), ('y', y)] : step (h, (i + 1, tail timeline)) (x, y) (x', y')
         where
            (x, y) = calculate h (x0, y0) (x'0, y'0)
            (x', y') = calculate' h (x0, y0) (x'0, y'0)

        result = array ('a', 'c') [('x', x0), ('y', y0)] : step (h, (1, tail timeline)) (x0, y0) (x'0, y'0)
    in (timeline, result)
-}

solve :: (Timegrid -> Problem -> Solution) -> Timegrid -> (VarValue, VarValue) -> Solution
solve method timegrid (x'0, y'0) = let r3 vars = sqrt (vars!'a' ** 2.0 + vars!'c' ** 2.0) ** 3.0

                                       coord_eq var vars = vars!chr (ord var + 1)-fst timegrid * g * m /r3 vars * vars!var
                                       x_eq _ = coord_eq 'a'
                                       y_eq _ = coord_eq 'c'

                                       deriv_eq var vars = (-g) * m / r3 vars * vars!var
                                       x'_eq _ = deriv_eq 'a'
                                       y'_eq _ = deriv_eq 'c'

                                       problem = listArray ('a', 'd') [(x0, x_eq), (x'0, x'_eq), (y0, y_eq), (y'0, y'_eq)]
                                    in method timegrid problem

solveExplicit :: Timegrid -> (VarValue, VarValue) -> Solution
solveExplicit = solve methodExplicit

solveTrapezoid :: Timegrid -> (VarValue, VarValue) -> Solution
solveTrapezoid = solve methodTrapezoid

solveImplicit :: Timegrid -> (VarValue, VarValue) -> Solution
solveImplicit = solve methodImplicit
