module NewtonSpec (spec) where

import Interpolation.Newton (
  newtonEvalAt,
  newtonRequiredWindowSize,
  newtonSafeInterval,
 )
import Test.Hspec
import Test.QuickCheck
import Types (
  Interval (..),
  NewtonConfig (..),
  Point (..),
  Window (..),
  WindowSize (..),
 )

approx :: Double -> Double -> Bool
approx a b = abs (a - b) <= 1e-9

mkXs :: Int -> Double -> Double -> [Double]
mkXs n start step = take n [start, start + step ..]

polyEval :: [Double] -> Double -> Double
polyEval coeffs x = sum [c * x ** i | (i, c) <- zip [0 ..] coeffs]

spec :: Spec
spec = describe "Newton interpolation" $ do
  it "requiredWindowSize reads from config" $ do
    newtonRequiredWindowSize (NewtonConfig (WindowSize 3)) `shouldBe` 3
    newtonRequiredWindowSize (NewtonConfig (WindowSize 5)) `shouldBe` 5

  describe "safeInterval" $ do
    it "even n=4 uses middle pair [x1,x2]" $ do
      let cfg = NewtonConfig (WindowSize 4)
          xs = [1, 2, 3, 4]
          w = Window [Point x (x * x) | x <- xs]
      newtonSafeInterval cfg w `shouldBe` Right (Interval 2 3)

    it "odd n=3 uses [x1,x2]" $ do
      let cfg = NewtonConfig (WindowSize 3)
          xs = [10, 20, 30]
          w = Window [Point x (x * x) | x <- xs]
      newtonSafeInterval cfg w `shouldBe` Right (Interval 20 30)

  describe "evalAt: exactness on polynomials of degree < n" $ do
    it "exact on cubic with n=4 (unit)" $ do
      let coeffs = [1, -2, 0.5, 0.1]
          cfg = NewtonConfig (WindowSize 4)
          xs = [-1, 0, 2, 5]
          w = Window [Point x (polyEval coeffs x) | x <- xs]
      case newtonSafeInterval cfg w of
        Left err -> expectationFailure ("safeInterval failed: " <> show err)
        Right (Interval l r) ->
          let xMid = (l + r) / 2
           in case newtonEvalAt cfg w xMid of
                Right y -> y `shouldSatisfy` approx (polyEval coeffs xMid)
                Left e -> expectationFailure ("newtonEvalAt failed: " <> show e)

    it "property: exact on quadratic with n=3" $
      property $ \(Finite c0) (Finite c1) (Finite c2) ->
        forAll genStartStep $ \(s, t) ->
          let cfg = NewtonConfig (WindowSize 3)
              xs = mkXs 3 s t
              w = Window [Point x (c0 + c1 * x + c2 * x * x) | x <- xs]
           in case newtonSafeInterval cfg w of
                Left err -> counterexample ("safeInterval failed: " <> show err) False
                Right (Interval l r) ->
                  forAll (choose (l, r)) $ \x ->
                    either
                      (const False)
                      (\y -> approx y (c0 + c1 * x + c2 * x * x))
                      (newtonEvalAt cfg w x)

newtype Finite = Finite Double deriving (Show)
instance Arbitrary Finite where
  arbitrary = Finite <$> suchThat arbitrary (\d -> not (isNaN d) && not (isInfinite d))

genStartStep :: Gen (Double, Double)
genStartStep = do
  s <- choose (-10, 10)
  t <- choose (0.5, 5) -- strictly positive step to keep xs strictly increasing
  pure (s, t)
