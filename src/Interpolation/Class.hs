module Interpolation.Class (
  Interpolator (..),
) where

import Types (Interval, Window, X, Y)

class Interpolator cfg where
  requiredWindowSize :: cfg -> Int
  safeInterval :: cfg -> Window -> Interval
  evalAt :: cfg -> Window -> X -> Y
