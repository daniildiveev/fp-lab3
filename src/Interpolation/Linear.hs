module Interpolation.Linear (
  linearRequiredWindowSize,
  linearSafeInterval,
  linearEvalAt,
) where

import Types (
  EngineError (..),
  Interval (..),
  LinearConfig (..),
  Point (..),
  Window (..),
  X,
  Y,
 )

linearRequiredWindowSize :: LinearConfig -> Int
linearRequiredWindowSize _ = 2

linearSafeInterval :: LinearConfig -> Window -> Either EngineError Interval
linearSafeInterval _ (Window (Point x1 _ : Point x2 _ : _))
  | x1 < x2 = Right (Interval x1 x2)
  | x2 < x1 = Right (Interval x2 x1)
  | otherwise = Left (EngineError "linearSafeInterval: degenerate segment (x1 == x2)")
linearSafeInterval _ _ =
  Left (EngineError "linearSafeInterval: window size must be exactly 2")

linearEvalAt :: LinearConfig -> Window -> X -> Either EngineError Y
linearEvalAt _ (Window (Point x1 y1 : Point x2 y2 : _)) x
  | x1 == x2 = Left (EngineError "linearEvalAt: degenerate segment (x1 == x2)")
  | otherwise = Right (y1 + (y2 - y1) * (x - x1) / (x2 - x1))
linearEvalAt _ _ _ =
  Left (EngineError "linearEvalAt: window size must be exactly 2")
