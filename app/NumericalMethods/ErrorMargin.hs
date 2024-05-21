{-
 `NumericalMethods.ErrorMargin` module implements functions that are useful in calculating error margin of numerical solutions.
-}
module NumericalMethods.ErrorMargin where

import Data.Array ((!))
import Times
import CauchyProblem
import NumericalMethods

{-
 Calculates absolute error between values of original function and numerical solution's one.
-}
absError :: ((a, a) -> Double) -> (Time -> a) -> (Time, a) -> Double
absError diff original_fn (t, val) = abs $ diff (original_fn t, val)

{-
 Returns list of taus where `tau_i = tau_{i-1} * k`.
-}
iterateTau :: Double -> Time -> [Time]
iterateTau k tau = [tau / (k ^ i) | i <- [0..] :: [Int]]

{-
    Tests numerical methods for their errors.
-}
testMethods :: ((Time, Time), Time, Double, Int) -> Problem -> (Time -> Double) -> [NumericalMethod] -> [[(Time, Double)]]
testMethods ((t0, t1), tau0, k, n) problem original methods = go
 where
    go = [zip (replicate (length methods) (0.0 - logBase 2 tau)) 
          [0.0 - logBase 2 (maximum  (zipWith (curry (absError (uncurry (-)) original)) timeline (map (! 'y') result))) | method <- methods, let (timeline, result) = method timegrid problem]
          | tau <- take n (iterateTau k tau0), let timegrid = createTimegrid (t0, t1) tau]
