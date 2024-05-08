{-
 `Task` module solves exact task numerically.
-}
module Task where

import CauchyProblem

r, re, g, m :: Float
r = 10.0 ** 4.0
re = 6380.0
g = 6.674 * 10 ** (-20.0)
m = 5.99 * (10.0 ** 24.0)

x0, y0 :: VarValue
x0 = r
y0 = 0.0

x'0 :: String -> VarValue
x'0 = read
y'0 :: String -> VarValue
y'0 str = read str + sqrt (g * m / r)

