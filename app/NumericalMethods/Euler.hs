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
    in (timeline, stepIter calculateU u0 (head timeline) (tail timeline))
