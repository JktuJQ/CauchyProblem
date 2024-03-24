{-
 `NumericalMethods.ErrorMargin` module implements functions that are useful in calculating error margin of numerical solutions.
-}
module NumericalMethods.ErrorMargin where
    
import CauchyProblem
import Times

{-
 Calculates absolute error between values of original function and numerical solution's one.
-}
absError :: (Time -> Float) -> (Time, VarValue) -> Float
absError original_fn (t, val) = abs $ original_fn t - val
{-
 Finds maximal error margin of numerical solution.
-}
maxErrorMargin :: (Time -> Float) -> [(Time, VarValue)] -> Float
maxErrorMargin original_fn numerical_solution = maximum $ map (absError original_fn) numerical_solution

{-
 Returns list of taus where `tau_i = tau_{i-1} / 2`.
-}
iterateTau :: Time -> Float -> [Time]
iterateTau tau 0 = [tau]
iterateTau tau i = tau : iterateTau (tau / 2) (i - 1)
