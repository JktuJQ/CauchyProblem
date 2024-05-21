{-
    `Task.Features` module implements task-specific functions that are used for
    detailed analysis.
-}
module Task.Features where

import Data.List (find)
import Data.Array((!))

import Times
import CauchyProblem
import Task
import Task.Solve

{-
    This function takes elements from solution until satellite falls down.
-}
stopAtFall :: [Vars] -> [Vars]
stopAtFall = takeWhile (\vars -> re <= distance (vars!'a', vars!'c')) 

{-
    Checks whether the initial speed is going to affect the satellite or not.
-}
check :: Timegrid -> SolveMethod -> ([Vars] -> [Vars]) -> (Vars -> Bool) -> (VarValue, VarValue) -> Bool
check timegrid method filtering checking (v0_x, v0_y) = any checking (filtering $ snd $ method timegrid (x'0 $ show v0_x, y'0 $ show v0_y))
{-
    Checks all values for speed until checks for satellite is affected critically and returns last value.
-}
performChecks :: ((VarValue, VarValue) -> Bool) -> ((Double, Double, Double), (Double, Double, Double)) -> Maybe (VarValue, VarValue)
performChecks checker ((v_min, v_max, v_step), (phi_min, phi_max, phi_step)) = find checker
    [(v * cos phi, v * sin phi) | v <- [v_min,v_min+v_step..v_max], phi <- [phi_min,phi_min+phi_step..phi_max]]