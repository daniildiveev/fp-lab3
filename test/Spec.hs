module Main (main) where

import Test.Hspec

import qualified CLISpec
import qualified EngineStreamSpec
import qualified LinearSpec
import qualified NewtonSpec
import qualified ParserSpec
import qualified PrinterSpec
import qualified ResamplerSpec
import qualified WindowSpec
import qualified ValidateSpec

main :: IO ()
main = hspec $ do
  ParserSpec.spec
  WindowSpec.spec
  LinearSpec.spec
  NewtonSpec.spec
  EngineStreamSpec.spec
  CLISpec.spec
  PrinterSpec.spec
  ResamplerSpec.spec
  ValidateSpec.spec
