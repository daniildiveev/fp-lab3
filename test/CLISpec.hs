module CLISpec (spec) where

import Test.Hspec
import System.Environment (withArgs)
import CLI (parseArgs)
import Types
  ( Config(..), Algorithm(..), LinearConfig(..)
  , NewtonConfig(..), WindowSize(..)
  , SamplerConfig(..), Step(..), StartMode(..)
  , Delimiter(..), Precision(..)
  )

spec :: Spec
spec = describe "CLI.parseArgs" $ do
  it "parses linear-only config with step and delim" $
    withArgs ["--linear","--step","0.7","--delim","space","--precision","4"] $ do
      cfg <- parseArgs
      algorithms cfg `shouldBe` [AlgorithmLinear LinearConfig]
      sampler cfg `shouldSatisfy` (\s -> step s == Step 0.7
                                      && startMode s == AlignFirst
                                      && precision s == Just (Precision 4))
      delimiter cfg `shouldBe` Space

  it "parses newton via -n without --linear" $
    withArgs ["-n","3","--step","1.0"] $ do
      cfg <- parseArgs
      algorithms cfg `shouldBe` [AlgorithmNewton (NewtonConfig (WindowSize 3))]
