module Interpolation.Linear
  ( linearRequiredWindowSize
  , linearSafeInterval
  , linearEvalAt
  ) where

import Types (Interval, LinearConfig, Window, X, Y)

linearRequiredWindowSize :: LinearConfig -> Int
linearRequiredWindowSize = undefined

linearSafeInterval :: LinearConfig -> Window -> Interval
linearSafeInterval = undefined

linearEvalAt :: LinearConfig -> Window -> X -> Y
linearEvalAt = undefined
