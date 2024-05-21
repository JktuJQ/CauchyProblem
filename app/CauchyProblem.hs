{-
 `CauchyProblem` module declares type aliases for problem-related characteristics.
-}
module CauchyProblem where

import Data.Array(Array, bounds, listArray, elems)

import Times

{-
 `VarName` type is an alias for `Char`.
-}
type VarName = Char
{-
 `VarValue` type is an alias for `Double`.
-}
type VarValue = Double
{-
 Array of variables.
-}
type Vars = Array VarName VarValue

{-
 Type of differential equation (`du/dt = Fn(t, u_i)`).

 Function must take time and variables stored into array as inputs and should output some value.
-}
type Fn = Time -> Vars -> Double
{-
 Array of differential equations.
-}
type Fns = Array VarName Fn

{-
 Pair of variable value at `t0` and corresponding differential equation.
-}
type Condition = (VarValue, Fn)

{-
 `Problem` type defines Cauchy problem entering conditions.

 Conditions are represented by an array with indexes of variable names and 
 content of that variable value at `t_start` with corresponding equation.
-}
type Problem = Array VarName Condition
{-
 Solution of Cauchy problem is a list of variable values at choosed time span.
-}
type Solution = (Timeline, [Vars])

{-
 Returns starting variable values for given problem.
-}
problemU0 :: Problem -> Vars
problemU0 = fmap fst
{-
 Returns functions for given problem.
-}
problemFns :: Problem -> Fns
problemFns = fmap snd

problemUnpack :: Problem -> (Vars, Fns)
problemUnpack problem = let to_array = listArray (bounds problem)
                            (a, b) = unzip $ elems problem
                        in (to_array a, to_array b)
