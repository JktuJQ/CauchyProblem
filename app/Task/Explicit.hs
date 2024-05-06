module Task.Explicit where

import Data.Array (array)

import Times
import CauchyProblem
import Task (re, g, m)

{-
    Solves exact task.
-}
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
        step (h, (_, [])) (x0, y0) (x'0, y'0) = [array ('x','y') [('x', x), ('y', y)]]
         where
            (x, y) = calculate h (x0, y0) (x'0, y'0)
        step (h, (i, timeline)) (x0, y0) (x'0, y'0) = array ('x','y') [('x', x), ('y', y)] : step (h, (i + 1, tail timeline)) (x, y) (x', y')
         where
            (x, y) = calculate h (x0, y0) (x'0, y'0)
            (x', y') = calculate' h (x0, y0) (x'0, y'0)

        result = array ('x', 'y') [('x', x0), ('y', y0)] : step (h, (1, tail timeline)) (x0, y0) (x'0, y'0)
    in (timeline, result)

