module Interpolation.Newton
  ( newtonRequiredWindowSize
  , newtonSafeInterval
  , newtonEvalAt
  ) where

import Types (Interval, NewtonConfig, Window, X, Y)

newtonRequiredWindowSize :: NewtonConfig -> Int
newtonRequiredWindowSize = undefined

newtonSafeInterval :: NewtonConfig -> Window -> Interval
newtonSafeInterval = undefined

newtonEvalAt :: NewtonConfig -> Window -> X -> Y
newtonEvalAt = undefined
