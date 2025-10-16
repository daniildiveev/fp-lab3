module Interpolation.Linear (
  linearRequiredWindowSize,
  linearSafeInterval,
  linearEvalAt,
) where

import Types (
  Interval (..),
  LinearConfig (..),
  Point (..),
  Window (..),
  X,
  Y,
 )

linearRequiredWindowSize :: LinearConfig -> Int
linearRequiredWindowSize _ = 2

linearSafeInterval :: LinearConfig -> Window -> Interval
linearSafeInterval _ (Window [Point x1 _, Point x2 _])
  | x1 <= x2 = Interval x1 x2
  | otherwise = Interval x2 x1
linearSafeInterval _ _ =
  error "linearSafeInterval: window size must be exactly 2"

linearEvalAt :: LinearConfig -> Window -> X -> Y
linearEvalAt _ (Window [Point x1 y1, Point x2 y2]) x
  | x2 == x1 = error "linearEvalAt: degenerate segment (x1 == x2)"
  | otherwise = y1 + (y2 - y1) * (x - x1) / (x2 - x1)
linearEvalAt _ _ _ =
  error "linearEvalAt: window size must be exactly 2"
