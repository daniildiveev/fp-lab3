module Stream.Resampler (
  ResamplerState (..),
  resample,
) where

import Types (Interval, StartMode, Step, X)

data ResamplerState = ResamplerState
  { rsStep :: Step
  , rsStart :: StartMode
  , rsCursor :: Maybe X
  }
  deriving (Show, Eq)

-- на вход: текущее состояние и интервал; на выход: сгенерированные X и новое состояние
resample :: ResamplerState -> Interval -> ([X], ResamplerState)
resample = undefined
