module Interpolation.Newton (
  newtonRequiredWindowSize,
  newtonSafeInterval,
  newtonEvalAt,
) where

import Types
  ( Interval (..)
  , NewtonConfig (..)
  , Point (..)
  , Window (..)
  , WindowSize (..)
  , X, Y
  , EngineError (..)
  )

newtonRequiredWindowSize :: NewtonConfig -> Int
newtonRequiredWindowSize (NewtonConfig (WindowSize n)) = n

newtonSafeInterval :: NewtonConfig -> Window -> Either EngineError Interval
newtonSafeInterval cfg (Window pts)
  | length pts < n =
      Left (EngineError "newtonSafeInterval: window smaller than required size")
  | otherwise =
      let xs = take n (map px pts)
      in case centerInterval xs of
           Just (l, r)
             | l < r     -> Right (Interval l r)
             | l > r     -> Right (Interval r l)
             | otherwise -> Left (EngineError "newtonSafeInterval: degenerate center interval (l == r)")
           Nothing       -> Left (EngineError "newtonSafeInterval: insufficient xs")
  where
    n = newtonRequiredWindowSize cfg

    centerInterval :: [X] -> Maybe (X, X)
    centerInterval xs =
      let m = length xs
      in case m of
           _ | m < 2      -> Nothing
             | even m     -> let i = m `div` 2 - 1
                                 j = m `div` 2
                             in pick xs i j
             | otherwise  -> let c = (m - 1) `div` 2
                             in pick xs c (c + 1)
    pick as i j =
      if i >= 0 && j < length as then Just (as !! i, as !! j) else Nothing

newtonEvalAt :: NewtonConfig -> Window -> X -> Either EngineError Y
newtonEvalAt cfg (Window pts) x
  | length pts < n =
      Left (EngineError "newtonEvalAt: window smaller than required size")
  | otherwise =
      let (xs, ys) = unzip (map (\p -> (px p, py p)) (take n pts))
      in do
        coeffs <- dividedDiffCoeffsE xs ys
        Right (evalNewton xs coeffs x)
  where
    n = newtonRequiredWindowSize cfg

dividedDiffCoeffsE :: [X] -> [Y] -> Either EngineError [Y]
dividedDiffCoeffsE xs ys =
  let m = length xs
      go j a acc
        | j == 0 = case a of
                     c0 : _ -> go 1 a (c0 : acc)
                     []     -> Right (reverse acc)
        | j == m = Right (reverse acc)
        | otherwise =
            let pairs = zip a (drop 1 a)
                stepAt i (aCur, aNext) = do
                  let den = xs !! (i + j) - xs !! i
                  if den == 0
                    then Left (EngineError "divided differences: duplicate X encountered")
                    else Right ((aNext - aCur) / den)
            in do
              a' <- sequence [ stepAt i p | (i, p) <- zip [0..] pairs, i + j < m ]
              case a' of
                c : _ -> go (j + 1) a' (c : acc)
                []    -> Right (reverse acc)
  in if null ys then Right [] else go 0 ys []

evalNewton :: [X] -> [Y] -> X -> Y
evalNewton xs coeffs x =
  case reverse (zip xs coeffs) of
    []              -> 0
    ((_, cj) : rest)-> foldl (\acc (xi, ci) -> ci + (x - xi) * acc) cj rest
