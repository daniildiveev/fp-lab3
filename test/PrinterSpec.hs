module PrinterSpec (spec) where

import Format.Printer (renderResult)
import Test.Hspec
import Types (AlgorithmTag (..), Precision (..), ResultPoint (..))

spec :: Spec
spec = describe "Format.Printer.renderResult" $ do
  it "formats linear prefix and fixed precision" $ do
    let s = renderResult (Precision 3) (ResultPoint LinearTag 1.2 3.4567)
    s `shouldBe` "linear: 1.200 3.457"

  it "formats newton prefix and fixed precision" $ do
    let s = renderResult (Precision 2) (ResultPoint NewtonTag 0.0 (10 / 3))
    s `shouldBe` "newton: 0.00 3.33"
