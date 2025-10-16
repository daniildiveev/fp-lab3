module Interpolation.Newton (
  newtonRequiredWindowSize,
  newtonSafeInterval,
  newtonEvalAt,
) where

import Types (Interval (..), NewtonConfig (..), Point (..), Window (..), WindowSize (..), X, Y)

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

dividedDiffCoeffs :: [X] -> [Y] -> [Y]
dividedDiffCoeffs xs ys =
  let n = length xs
      go j a acc
        | j == 0 = go 1 a (head a : acc)
        | j == n = reverse acc
        | otherwise =
            let a' = [(a !! (i + 1) - a !! i) / (xs !! (i + j) - xs !! i) | i <- [0 .. n - j - 1]]
             in go (j + 1) a' (head a' : acc)
   in if null ys then [] else go 0 ys []

evalNewton :: [X] -> [Y] -> X -> Y
evalNewton xs coeffs x =
  case reverse (zip xs coeffs) of
    [] -> 0
    ((xj, cj) : rest) -> foldl (\acc (xi, ci) -> ci + (x - xi) * acc) cj rest
