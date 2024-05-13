{-
 `NumericalMethods.ErrorMargin` module implements functions that are useful in calculating error margin of numerical solutions.
-}
module NumericalMethods.ErrorMargin where

import Times

{-
 Calculates absolute error between values of original function and numerical solution's one.
-}
absError :: ((a, a) -> Float) -> (Time -> a) -> (Time, a) -> Float
absError diff original_fn (t, val) = abs $ diff (original_fn t, val)

{-
 Returns list of taus where `tau_i = tau_{i-1} * k`.
-}
iterateTau :: Float -> Time -> [Time]
iterateTau k tau = [tau / (k ^ i) | i <- [0..] :: [Int]]
