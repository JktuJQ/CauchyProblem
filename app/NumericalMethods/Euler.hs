{-
 `NumericalMethods.Euler` module implements Euler's numerical methods.
-}
module NumericalMethods.Euler where

import Data.Array(bounds, indices, listArray, (!))

import CauchyProblem
import Times
import NumericalMethods

{-
 `simpleStep` calculates next item based on derivative approximation.
-}
simpleStep :: Time -> Fns -> IterMethod
simpleStep tau fns prev_u t = listArray (bounds prev_u) [(prev_u!name) + tau * (fns!name) t prev_u | name <- indices prev_u]

{-
 Implements simplest Euler's explicit numerical method for Cauchy problem.
-}
methodExplicit :: Timegrid -> Problem -> Solution
methodExplicit (tau, timeline) problem =
    let u0 = problemU0 problem
        fns = problemFns problem

        step = stepIter $ simpleStep tau fns
    in (timeline, step u0 (head timeline) (tail timeline))

methodTrapezoid :: Timegrid -> Problem -> Solution
methodTrapezoid (tau, timeline) problem =
    let u0 = problemU0 problem
        fns = problemFns problem

        calculateU :: IterMethod -> IterMethod
        calculateU calculate_inter_u prev_u t =
            listArray (bounds prev_u)
                [(prev_u!name) + (tau / 2.0) * ((fns!name) t prev_u + (fns!name) (t + tau) (calculate_inter_u prev_u t))| name <- indices prev_u]

        step = stepIter $ calculateU $ simpleStep tau fns
    in (timeline, step u0 (head timeline) (tail timeline))
