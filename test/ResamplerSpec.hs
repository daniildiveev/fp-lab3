module ResamplerSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Types (Interval(..), StartMode(..), Step(..))
import Stream.Resampler (ResamplerState(..), resample)

approx :: Double -> Double -> Bool
approx a b = abs (a - b) <= 1e-12

approxList :: [Double] -> [Double] -> Bool
approxList as bs = length as == length bs && and (zipWith approx as bs)

mkState :: Double -> StartMode -> Maybe Double -> ResamplerState
mkState s mode cur = ResamplerState { rsStep = Step s, rsStart = mode, rsCursor = cur }

spec :: Spec
spec = describe "Stream.Resampler.resample" $ do
  it "AlignFirst: starts from grid-aligned point ≥ L" $ do
    let st  = mkState 0.5 AlignFirst Nothing
        iv  = Interval 0.1 1.2
        (xs, st') = resample st iv
    xs `shouldSatisfy` approxList [0.5, 1.0]
    st' `shouldSatisfy` (\s -> rsCursor s == Just 1.5)

  it "PassThrough: starts exactly at L when cursor is Nothing" $ do
    let st  = mkState 0.5 PassThrough Nothing
        iv  = Interval 0.1 1.2
        (xs, st') = resample st iv
    xs `shouldSatisfy` approxList [0.1, 0.6, 1.1]
    st' `shouldSatisfy` (\s -> rsCursor s == Just 1.6)

  it "does not duplicate points across consecutive intervals (cursor respected)" $ do
    let st0 = mkState 0.5 AlignFirst Nothing
        (xs1, st1) = resample st0 (Interval 0.0 1.0)
        (xs2, _  ) = resample st1 (Interval 1.0 1.5)
    xs1 `shouldSatisfy` approxList [0.0, 0.5, 1.0]
    xs2 `shouldSatisfy` approxList [1.5]

  it "property: all emitted xs lie in [L,R] (with tiny epsilon)" $
    property $ \l rRaw sRaw ->
      let r = l + abs rRaw + 1e-6
          s = max 1e-6 (abs sRaw)  -- step > 0
          st  = mkState s AlignFirst Nothing
          (xs, _) = resample st (Interval l r)
      in all (\x -> x >= l - 1e-9 && x <= r + 1e-9) xs
