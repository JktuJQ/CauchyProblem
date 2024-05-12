{-
 `NumericalMethods.Methods` module implements numerical methods.
-}
module NumericalMethods.Methods where

import Data.Array(bounds, indices, listArray, (!))

import CauchyProblem
import Times
import NumericalMethods

{-
 `simpleStep` calculates next item based on derivative approximation.
-}
simpleStep :: Time -> Fns -> IterMethod
simpleStep tau fns prev_u t = listArray (bounds prev_u) [(prev_u!name)
                                                         + tau * (fns!name) t prev_u
                                                         | name <- indices prev_u]

{-
 Implementation of simplest Euler's explicit numerical method for Cauchy problem.
-}
methodExplicit :: NumericalMethod
methodExplicit (tau, timeline) problem =
    let (u0, fns) = problemUnpack problem
        step = stepIter $ simpleStep tau fns
    in (timeline, step u0 (head timeline) (tail timeline))

{-
 Implementation of trapezoid Euler's explicit numerical method for Cauchy problem.
-}
methodTrapezoid :: NumericalMethod
methodTrapezoid (tau, timeline) problem =
    let (u0, fns) = problemUnpack problem

        calculateU :: IterMethod -> IterMethod
        calculateU calculate_inter_u prev_u t =
            listArray (bounds prev_u)
                [(prev_u!name) + (tau / 2.0) * ((fns!name) t prev_u +
                 (fns!name) (t + tau) (calculate_inter_u prev_u t))
                 | name <- indices prev_u]

        step = stepIter $ calculateU $ simpleStep tau fns
    in (timeline, step u0 (head timeline) (tail timeline))

{-
  Implementation of Runge-Kutta method for Cauchy problem.
-}
methodRungeKutta :: NumericalMethod
methodRungeKutta (tau, timeline) problem =
    let (u0, fns) = problemUnpack problem

        add_array array1 array2 k = listArray (bounds array1) [(array1!i) + k * (array2!i) | i <- indices array1]

        calculateU :: IterMethod
        calculateU prev_u t = add_array prev_u k (tau / 6.0)
         where
            k1, k2, k3, k4, k :: Vars
            k1 = listArray (bounds prev_u) [(fns!i) t prev_u | i <- indices prev_u]
            k2 = listArray (bounds prev_u) [(fns!i) (t + tau / 2.0) (add_array prev_u k1 (tau / 2.0))
                                            | i <- indices prev_u]
            k3 = listArray (bounds prev_u) [(fns!i) (t + tau / 2.0) (add_array prev_u k2 (tau / 2.0))
                                            | i <- indices prev_u]
            k4 = listArray (bounds prev_u) [(fns!i) (t + tau) (add_array prev_u k3 tau)
                                            | i <- indices prev_u]

            k = add_array (add_array (add_array k1 k2 2.0) k3 2.0) k4 1.0
        
        step = stepIter calculateU
    in (timeline, step u0 (head timeline) (tail timeline))
