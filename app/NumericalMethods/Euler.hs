{-
 `NumericalMethods.Euler` module implements Euler's numerical methods.
-}
module NumericalMethods.Euler where

import Data.Array(bounds, listArray, (!))

import CauchyProblem
import Times
import NumericalMethods

{-
 Implements simplest Euler's explicit numerical method for Cauchy problem.
-}
methodExplicit :: Timegrid -> Problem -> Solution
methodExplicit (tau, timeline) problem =
    let (start, end) = bounds problem 
        u0 = problemU0 problem
        fns = problemFns problem

        calculateU :: IterMethod
        calculateU prev_u t = listArray (start, end) [(prev_u!name) + tau * (fns!name) t prev_u | name <- [start..end]]

        step = stepIter calculateU 
    in (timeline, step u0 (head timeline) (tail timeline))

methodTrapezoid :: Timegrid -> Problem -> Solution
methodTrapezoid (tau, timeline) problem =
    let (start, end) = bounds problem
        u0 = problemU0 problem
        fns = problemFns problem

        intermediateU :: IterMethod
        intermediateU prev_u t = listArray (start, end) [(prev_u!name) + tau * (fns!name) t prev_u | name <- [start..end]]

        calculateU :: IterMethod
        calculateU = calculateU' intermediateU
         where
            calculateU' :: IterMethod -> IterMethod
            calculateU' calculate_inter_u prev_u' t' =
                listArray (start, end)
                    [(prev_u'!name) + (tau / 2.0) * ((fns!name) t' prev_u' + (fns!name) (t' + tau) (calculate_inter_u prev_u' t'))| name <- [start..end]]

        step = stepIter calculateU 
    in (timeline, step u0 (head timeline) (tail timeline))
