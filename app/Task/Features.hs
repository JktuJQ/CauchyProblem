{-
    `Task.Features` module implements task-specific functions that are used for
    detailed analysis.
-}
module Task.Features where

import Data.Array(Array, (!))

import Times
import CauchyProblem
import NumericalMethods.ErrorMargin
import Task
import Task.Solve

{-
    This function takes elements from solution until satellite falls down.
-}
stopAtFall :: [Vars] -> [Vars]
stopAtFall = takeWhile (\vars -> re <= distance (vars!'a', vars!'c')) 

{-
    Checks whether the initial speed is going to crash the satellite or not.
-}
checkForFall :: Timegrid -> SolveMethod -> (VarValue, VarValue) -> Bool
checkForFall timegrid method (v0_x, v0_y) = length (snd timegrid) /=
    length (stopAtFall (snd $ method timegrid (x'0 $ show v0_x, y'0 $ show v0_y)))

{-
    Checks all values for speed until satellite crashes and returns last value (when the satellite crashed).
-}
performChecks :: Timegrid -> SolveMethod -> ((Float, Float, Float), (Float, Float, Float)) -> (VarValue, VarValue)
performChecks timegrid method ((v_min, v_max, v_step), (phi_min, phi_max, phi_step)) = takeOneAfter (not . checkForFall timegrid method)
    [(v * cos phi, v * sin phi) | v <- [v_min,v_min+v_step..v_max], phi <- [phi_min,phi_min+phi_step..phi_max]]
 where
    takeOneAfter :: ((VarValue, VarValue) -> Bool) -> [(VarValue, VarValue)] -> (VarValue, VarValue)
    takeOneAfter predicate xs = go True (head xs) (tail xs)
     where
        go True x l = go (predicate x) (if predicate x then head l else x) (tail l)
        go False x _ = x

