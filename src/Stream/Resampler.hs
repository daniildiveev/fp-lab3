module Stream.Resampler (
  ResamplerState (..),
  resample,
) where

import Types (Interval (..), StartMode (..), Step (..), X)

data ResamplerState = ResamplerState
  { rsStep :: Step
  , rsStart :: StartMode
  , rsCursor :: Maybe X
  }
  deriving (Show, Eq)

resample :: ResamplerState -> Interval -> ([X], ResamplerState)
resample st (Interval l r)
  | l > r + eps = ([], st)
  | otherwise =
      let x0 =
            case (rsCursor st, rsStart st) of
              (Nothing, AlignFirst) -> ceilToGrid 0 step l
              (Nothing, PassThrough) -> l
              (Just cur, _) -> max l (ceilFrom cur step)

          xs =
            if x0 > r + eps
              then []
              else takeWhile (<= r + eps) (iterate (+ step) x0)

          lastM = foldl (\_ a -> Just a) Nothing xs

          newC = case lastM of
            Nothing -> rsCursor st
            Just lx -> Just (lx + step)
       in (xs, st {rsCursor = newC})
 where
  step = unStep (rsStep st)
  eps = 1e-12 * max step 1

ceilToGrid :: X -> X -> X -> X
ceilToGrid base step x =
  let t = (x - base - tol) / step
      k = (ceiling t :: Integer)
   in base + fromIntegral k * step
 where
  tol = 1e-12 * max step 1

ceilFrom :: X -> X -> X
ceilFrom cur step = ceilToGrid cur step cur
