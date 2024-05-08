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
 Implementation of simplest Euler's explicit numerical method for Cauchy problem.
-}
methodExplicit :: Timegrid -> Problem -> Solution
methodExplicit (tau, timeline) problem =
    let (u0, fns) = problemUnpack problem
        step = stepIter $ simpleStep tau fns
    in (timeline, step u0 (head timeline) (tail timeline))

{-
 Implementation of trapezoid Euler's explicit numerical method for Cauchy problem.
-}
methodTrapezoid :: Timegrid -> Problem -> Solution
methodTrapezoid (tau, timeline) problem =
    let (u0, fns) = problemUnpack problem

        calculateU :: IterMethod -> IterMethod
        calculateU calculate_inter_u prev_u t =
            listArray (bounds prev_u)
                [(prev_u!name) + (tau / 2.0) * ((fns!name) t prev_u + (fns!name) (t + tau) (calculate_inter_u prev_u t)) | name <- indices prev_u]

        step = stepIter $ calculateU $ simpleStep tau fns
    in (timeline, step u0 (head timeline) (tail timeline))

{-
 Implementation of Euler's numerical implicit method for Cauchy problem.
-}
methodImplicit :: Timegrid -> Problem -> Solution
methodImplicit (tau, timeline) problem =
    let (u0, fns) = problemUnpack problem

        calculateU :: IterMethod -> IterMethod
        calculateU calculate_inter_u prev_u t =
            listArray (bounds prev_u)
                [(prev_u!name) + tau * (fns!name) (t + tau) (calculate_inter_u prev_u t) | name <- indices prev_u]

        step = stepIter $ calculateU $ simpleStep tau fns
    in (timeline, step u0 (head timeline) (tail timeline))
