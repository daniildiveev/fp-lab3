module Interpolation.Newton (
  newtonRequiredWindowSize,
  newtonSafeInterval,
  newtonEvalAt,
) where

import Types (
  Interval (..),
  NewtonConfig (..),
  Point (..),
  Window (..),
  WindowSize (..),
  X,
  Y,
 )

newtonRequiredWindowSize :: NewtonConfig -> Int
newtonRequiredWindowSize (NewtonConfig (WindowSize n)) = n

newtonSafeInterval :: NewtonConfig -> Window -> Interval
newtonSafeInterval cfg (Window pts)
  | length pts < n = error "newtonSafeInterval: window smaller than required size"
  | otherwise =
      let xs = map px (take n pts)
       in case n `mod` 2 of
            0 ->
              let m = n `div` 2
                  l = m - 1
                  r = m
               in Interval (xs !! l) (xs !! r)
            _ ->
              let c = (n - 1) `div` 2
               in Interval (xs !! c) (xs !! (c + 1))
 where
  n = newtonRequiredWindowSize cfg

newtonEvalAt :: NewtonConfig -> Window -> X -> Y
newtonEvalAt cfg (Window pts) x
  | length pts < n = error "newtonEvalAt: window smaller than required size"
  | otherwise =
      let (xs, ys) = unzip [(px p, py p) | p <- take n pts]
          coeffs = dividedDiffCoeffs xs ys
       in evalNewton xs coeffs x
 where
  n = newtonRequiredWindowSize cfg

-- No partials; no unused bindings.
dividedDiffCoeffs :: [X] -> [Y] -> [Y]
dividedDiffCoeffs xs ys =
  let n = length xs
      go :: Int -> [Y] -> [Y] -> [Y]
      go j a acc
        | j == 0 =
            case a of
              c0 : _ -> go 1 a (c0 : acc) -- c0 = f[x0]
              [] -> reverse acc
        | j == n = reverse acc
        | otherwise =
            let pairs = zip a (drop 1 a)
                a' =
                  [ (aNext - aCur) / (xs !! (i + j) - xs !! i)
                  | (i, (aCur, aNext)) <- zip [0 ..] pairs
                  , i + j < n
                  ]
             in case a' of
                  c : _ -> go (j + 1) a' (c : acc)
                  [] -> reverse acc
   in if null ys then [] else go 0 ys []

evalNewton :: [X] -> [Y] -> X -> Y
evalNewton xs coeffs x =
  case reverse (zip xs coeffs) of
    [] -> 0
    ((_, cj) : rest) -> foldl (\acc (xi, ci) -> ci + (x - xi) * acc) cj rest
