{-
 `Task` module solves exact task numerically.
-}
module Task where

import CauchyProblem

cosmicSpeed :: Float -> Float
cosmicSpeed radius = sqrt (g * m / radius)

distance :: (Float, Float) -> Float
distance (x, y) = sqrt (x ** 2.0 + y ** 2.0)

r, re, g, m, v1c :: Float
r = 10.0 ** 4.0  -- km
re = 6380.0  -- km
g = 6.674 * 10 ** (-20.0)  -- km^3 * kg^-1 * s^-2
m = 5.99 * (10.0 ** 24.0)  -- kg
v1c = cosmicSpeed r  -- km * s^-1

x0, y0 :: VarValue
x0 = r  -- km
y0 = 0.0  -- km

x'0 :: String -> VarValue
x'0 str = if str == "" then 0.0 else read str  -- km * s^-1
y'0 :: String -> VarValue
y'0 str = (if str == "" then 0.0 else read str) + v1c  -- km * s^-1
