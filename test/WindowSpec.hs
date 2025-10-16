module WindowSpec (spec) where

import Stream.SlidingWindow (slidingWindows)
import Test.Hspec
import Test.QuickCheck
import qualified Types as T
import Util.Validate (validateMonotonic)

spec :: Spec
spec = describe "Windows & Monotonicity" $ do
  describe "slidingWindows (unit)" $ do
    it "creates overlapping windows of size 3" $ do
      let pts = [T.Point x (x * x) | x <- [1 .. 5]]
          ws = slidingWindows 3 pts
          xsInWindows = map (map (\(T.Point x _) -> x) . (\(T.Window w) -> w)) ws
      length ws `shouldBe` 3
      xsInWindows `shouldBe` [[1, 2, 3], [2, 3, 4], [3, 4, 5]]

    it "returns [] for k <= 0" $ do
      slidingWindows 0 [T.Point 1 1] `shouldBe` []
      slidingWindows (-1) [T.Point 1 1] `shouldBe` []

  describe "slidingWindows (property)" $ do
    it "length is max(0, n-k+1)" $
      property $ \(Positive n0) (Positive k0) ->
        let n = min 50 n0
            k = min 20 k0
            xs = [1 .. fromIntegral n]
            pts = [T.Point x x | x <- xs]
            ws = slidingWindows k pts
            expected = if k <= 0 then 0 else max 0 (n - k + 1)
         in length ws == expected

  describe "validateMonotonic" $ do
    it "accepts strictly increasing px" $
      validateMonotonic [T.Point 1 0, T.Point 2 0, T.Point 3 0]
        `shouldBe` Right [T.Point 1 0, T.Point 2 0, T.Point 3 0]

    it "rejects duplicate X" $
      validateMonotonic [T.Point 1 0, T.Point 1 1] `shouldBe` Left (T.DuplicateX 1)

    it "rejects non-increasing X" $
      validateMonotonic [T.Point 2 0, T.Point 1 0] `shouldSatisfy` isNonMono
 where
  isNonMono (Left (T.NonMonotonicX _ _)) = True
  isNonMono _ = False
