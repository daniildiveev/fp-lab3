module Format.Printer (renderResult) where

import Numeric (showFFloat)
import Types (AlgorithmTag (..), Precision (..), ResultPoint (..))

renderResult :: Precision -> ResultPoint -> String
renderResult (Precision p) (ResultPoint tag x y) =
  prefix tag ++ " " ++ fmt x ++ " " ++ fmt y
 where
  fmt v = showFFloat (Just p) v ""
  prefix t = case t of
    LinearTag -> "linear:"
    NewtonTag -> "newton:"
