module LinearSpec (spec) where

import Interpolation.Linear
  ( linearEvalAt
  , linearRequiredWindowSize
  , linearSafeInterval
  )
import Test.Hspec
import Test.QuickCheck
import Types
  ( Interval (..)
  , LinearConfig (..)
  , Point (..)
  , Window (..)
  )

approx :: Double -> Double -> Bool
approx a b = abs (a - b) <= 1e-12

spec :: Spec
spec = describe "Linear interpolation" $ do
  it "requiredWindowSize = 2" $
    linearRequiredWindowSize LinearConfig `shouldBe` 2

  it "safeInterval is the segment between two points" $
    linearSafeInterval LinearConfig (Window [Point 1 10, Point 3 30])
      `shouldBe` Right (Interval 1 3)

  it "evaluates exactly on a line y = a*x + b" $ do
    let a = 2
        b = 1
        w = Window [Point 0 (a*0 + b), Point 10 (a*10 + b)]
    case linearEvalAt LinearConfig w 5 of
      Right yMid -> yMid `shouldSatisfy` approx (a*5 + b)
      Left err   -> expectationFailure ("linearEvalAt failed: " <> show err)

  it "property: for any a,b and x in [x1,x2], eval matches y=a*x+b" $
    property $ \(Finite a) (Finite b) ->
      let x1 = 1.0 :: Double
          x2 = 4.0
          w  = Window [Point x1 (a*x1 + b), Point x2 (a*x2 + b)]
      in forAll (choose (x1, x2)) $ \x ->
           either
             (const False)
             (\y -> approx y (a*x + b))
             (linearEvalAt LinearConfig w x)

newtype Finite = Finite Double deriving (Show)
instance Arbitrary Finite where
  arbitrary = Finite <$> suchThat arbitrary (\d -> not (isNaN d) && not (isInfinite d))
