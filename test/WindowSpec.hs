module WindowSpec (spec) where

import Stream.SlidingWindow (slidingWindows)
import Test.Hspec
import Test.QuickCheck
import Types (Point (..), ValidationError (..), Window (..))
import Util.Validate (validateMonotonic)

spec :: Spec
spec = describe "Windows & Monotonicity" $ do
  describe "slidingWindows (unit)" $ do
    it "creates overlapping windows of size 3" $ do
      let pts = [Point x (x * x) | x <- [1 .. 5]]
          ws = slidingWindows 3 pts
      length ws `shouldBe` 3
      map (map px . unWindow) ws `shouldBe` [[1, 2, 3], [2, 3, 4], [3, 4, 5]]

    it "returns [] for k <= 0" $ do
      slidingWindows 0 [Point 1 1] `shouldBe` []
      slidingWindows (-1) [Point 1 1] `shouldBe` []

  describe "slidingWindows (property)" $ do
    it "length is max(0, n-k+1)" $
      property $ \(Positive n0) (Positive k0) ->
        let n = min 50 n0 -- keep tests small
            k = min 20 k0
            xs = [1 .. fromIntegral n]
            pts = [Point x x | x <- xs]
            ws = slidingWindows k pts
            expected = if k <= 0 then 0 else max 0 (n - k + 1)
         in length ws == expected

  describe "validateMonotonic" $ do
    it "accepts strictly increasing px" $
      validateMonotonic [Point 1 0, Point 2 0, Point 3 0] `shouldBe` Right [Point 1 0, Point 2 0, Point 3 0]

    it "rejects duplicate X" $
      validateMonotonic [Point 1 0, Point 1 1] `shouldBe` Left (DuplicateX 1)

    it "rejects non-increasing X" $
      validateMonotonic [Point 2 0, Point 1 0] `shouldSatisfy` isNonMono
 where
  isNonMono (Left (NonMonotonicX _ _)) = True
  isNonMono _ = False

-- bring px/unWindow into scope (pattern usage in Hspec expectation)
px (Point x _) = x
unWindow (Window w) = w
