{-
 `Times` module declares type aliases for time representation.
-}
module Times where

{-
 `Time` type is an alias for `Double`.
-}
type Time = Double
{-
 `Timeline` is an alias for list of time values.
-}
type Timeline = [Time]
{-
 `Timegrid` is an alias for tuple of tau and timeline wit.
-}
type Timegrid = (Time, Timeline)
{-
 Type of time characteristic.
 First value should be `t0` (start point) and the second should be `t1` (end point).
-}
type TimeSettings = (Time, Time)

{-
 `createTimegrid` function generates `Timegrid` with given tau as a step.
-}
createTimegrid :: TimeSettings -> Time -> Timegrid
createTimegrid (t0, t1) tau = (tau, [t0 + tau * fromIntegral n | n <- [0 :: Int .. round $ (t1 - t0) / tau]])
