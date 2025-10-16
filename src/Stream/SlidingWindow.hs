module Stream.SlidingWindow (slidingWindows) where

import Data.List (tails)
import Types (Point (..), Window (..))

slidingWindows :: Int -> [Point] -> [Window]
slidingWindows k pts
  | k <= 0 = []
  | otherwise =
      [ Window w
      | t <- tails pts
      , let w = take k t
      , length w == k
      ]
