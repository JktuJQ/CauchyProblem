module Task.Features where

import Data.Array((!))

import Times
import CauchyProblem
import Task
import Task.Solve

{-
    By exploiting lazy evaluation, this function takes elements from solution until satellite falls down.
-}
stopAtFall :: [Vars] -> [Vars]
stopAtFall = takeWhile (\vars -> re <= distance (vars!'a', vars!'c')) 

{-
    Checks whether the initial speed is going to crash the satellite or not.
-}
checkForFall :: SolveMethod -> (VarValue, VarValue) -> Bool
checkForFall method (v0_x, v0_y) = 86401 /= length (stopAtFall (snd $ method (createTimegrid (0.0, 86400.0) 1.0) (x'0 $ show v0_x, y'0 $ show v0_y)))

{-
    Checks all values for speed until satellite crashes and returns last value (when the satellite crashed).
-}
performChecks :: SolveMethod -> ((VarValue, VarValue, Float), (VarValue, VarValue, Float)) -> (VarValue, VarValue)
performChecks method ((v0_x, till_v0_x, step_x), (v0_y, till_v0_y, step_y)) = takeOneAfter (not . checkForFall method) [(v_x, v_y) | v_x <- [v0_x,v0_x+step_x..till_v0_x], v_y <- [v0_y,v0_y+step_y..till_v0_y]]
 where
    takeOneAfter :: ((VarValue, VarValue) -> Bool) -> [(VarValue, VarValue)] -> (VarValue, VarValue)
    takeOneAfter predicate xs = go True (head xs) (tail xs)
     where
        go True x l = go (predicate x) (if predicate x then head l else x) (tail l)
        go False x _ = x

