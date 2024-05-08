{-
 `NumericalMethods` module declares functions that are used in most of numerical methods.
-}
module NumericalMethods where

import CauchyProblem
import Times

{-
 `IterMethod` type represents functions which implement iterative numerical methods.

 Iterative numerical method should be inductive,
 so by taking previous state and new time it should be able to produce new state.
-}
type IterMethod = Vars -> Time -> Vars

{-
    `NumericalMethod` type alias represents numerical methods that are giving solution
    for exact problem by iterating over a timegrid.
-}
type NumericalMethod = Timegrid -> Problem -> Solution

{-
 `stepIter` function inductively calculates result of iterative numerical method
  by iterating on a `Timeline`.
-}
stepIter :: IterMethod -> Vars -> Time -> Timeline -> [Vars]
stepIter f prev_step t [] = [f prev_step t]
stepIter f prev_step t new_timeline = prev_step : stepIter f (f prev_step t) (head new_timeline) (tail new_timeline)
