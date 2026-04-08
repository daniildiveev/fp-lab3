module Util.Validate (
  validateMonotonic,
  validateConfig,
  stepValidate,
) where

import Types (
  Algorithm (..),
  Config (..),
  NewtonConfig (..),
  Point (..),
  Precision (..),
  SamplerConfig (..),
  Step (..),
  ValidationError (..),
  WindowSize (..),
  X,
 )

validateMonotonic :: [Point] -> Either ValidationError [Point]
validateMonotonic xs =
  case firstBad xs of
    Nothing -> Right xs
    Just (p, q)
      | px q == px p -> Left (DuplicateX (px q))
      | otherwise -> Left (NonMonotonicX (px p) (px q))
 where
  firstBad :: [Point] -> Maybe (Point, Point)
  firstBad (a : b : rest)
    | px b <= px a = Just (a, b)
    | otherwise = firstBad (b : rest)
  firstBad _ = Nothing

validateConfig :: Config -> Either ValidationError Config
validateConfig cfg@(Config algos _ (SamplerConfig (Step s) _ mPrec)) = do
  if null algos then Left (GeneralValidationError "no algorithms selected") else Right ()
  if s <= 0 || isNaN s || isInfinite s then Left (BadStep s) else Right ()
  case mPrec of
    Just (Precision p) | p < 0 -> Left (GeneralValidationError "precision must be non-negative")
    _ -> Right ()
  mapM_ checkAlgo algos
  Right cfg
 where
  checkAlgo :: Algorithm -> Either ValidationError ()
  checkAlgo (AlgorithmLinear _) = Right ()
  checkAlgo (AlgorithmNewton (NewtonConfig (WindowSize n)))
    | n < 2 = Left (BadWindowSize n)
    | otherwise = Right ()

stepValidate :: Maybe X -> Point -> Either ValidationError (Maybe X)
stepValidate Nothing (Point x _) = Right (Just x)
stepValidate (Just prevX) (Point x _)
  | x == prevX = Left (DuplicateX x)
  | x < prevX = Left (NonMonotonicX prevX x)
  | otherwise = Right (Just x)
