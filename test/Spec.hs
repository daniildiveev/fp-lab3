module Main (main) where

import qualified EngineSpec
import qualified LinearSpec
import qualified NewtonSpec
import qualified ParserSpec
import Test.Hspec
import qualified WindowSpec

main :: IO ()
main = hspec $ do
  ParserSpec.spec
  WindowSpec.spec
  LinearSpec.spec
  NewtonSpec.spec
  EngineSpec.spec
