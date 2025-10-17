module ValidateSpec (spec) where

import Test.Hspec
import Types (Point (..))
import Util.Validate (stepValidate)

spec :: Spec
spec = describe "Util.Validate.stepValidate" $ do
  it "accepts strictly increasing x" $ do
    let pts = [Point 1 0, Point 2 0, Point 3 0]
        res = foldl (\acc p -> acc >>= \mx -> stepValidate mx p) (Right Nothing) pts
    res `shouldSatisfy` isRightE
  it "rejects duplicate x" $ do
    let pts = [Point 1 0, Point 1 1]
        res = foldl (\acc p -> acc >>= \mx -> stepValidate mx p) (Right Nothing) pts
    res `shouldSatisfy` isLeftE
  it "rejects non-increasing x" $ do
    let pts = [Point 2 0, Point 1 0]
        res = foldl (\acc p -> acc >>= \mx -> stepValidate mx p) (Right Nothing) pts
    res `shouldSatisfy` isLeftE

isRightE :: Either a b -> Bool
isRightE (Right _) = True
isRightE _ = False

isLeftE :: Either a b -> Bool
isLeftE (Left _) = True
isLeftE _ = False
