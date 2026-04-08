module EngineStreamSpec (spec) where

import Data.Bifunctor (bimap)
import Engine.Stream (flushEngine, initEngineState, stepEngine)
import Test.Hspec
import Test.QuickCheck
import Types (
  Algorithm (..),
  AlgorithmTag (..),
  Config (..),
  Delimiter (..),
  LinearConfig (..),
  NewtonConfig (..),
  Point (..),
  Precision (..),
  ResultPoint (..),
  SamplerConfig (..),
  StartMode (..),
  Step (..),
  WindowSize (..),
 )

approx :: Double -> Double -> Bool
approx a b = abs (a - b) <= 1e-9

approxList :: [Double] -> [Double] -> Bool
approxList as bs = length as == length bs && and (zipWith approx as bs)

cfgLinear :: Double -> Config
cfgLinear stepV =
  Config
    { algorithms = [AlgorithmLinear LinearConfig]
    , delimiter = Space
    , sampler =
        SamplerConfig
          { step = Step stepV
          , startMode = AlignFirst
          , precision = Just (Precision 6)
          }
    }

cfgBoth :: Double -> Config
cfgBoth stepV =
  Config
    { algorithms =
        [ AlgorithmLinear LinearConfig
        , AlgorithmNewton (NewtonConfig (WindowSize 3))
        ]
    , delimiter = Space
    , sampler =
        SamplerConfig
          { step = Step stepV
          , startMode = AlignFirst
          , precision = Just (Precision 6)
          }
    }

cfgNewton3 :: Double -> Config
cfgNewton3 stepV =
  Config
    { algorithms = [AlgorithmNewton (NewtonConfig (WindowSize 3))]
    , delimiter = Space
    , sampler =
        SamplerConfig
          { step = Step stepV
          , startMode = AlignFirst
          , precision = Just (Precision 6)
          }
    }

runStream :: Config -> [Point] -> [ResultPoint]
runStream cfg pts =
  let go _ acc [] = reverse acc
      go st0 acc (q : qs) =
        let (st1, out) = stepEngine cfg st0 q
         in go st1 (reverse out ++ acc) qs
      st = initEngineState cfg
      produced = go st [] pts
      flushed = flushEngine cfg (initStateAfter cfg pts)
   in produced ++ flushed
 where
  initStateAfter c ps =
    let advance (st0, _) = stepEngine c st0
        (stF, _) = foldl advance (initEngineState c, []) ps
     in stF

spec :: Spec
spec = describe "Engine.Stream (streaming)" $ do
  it "Linear + step 0.7 yields 0,0.7,1.4,2.1,2.8 on y=x when points arrive incrementally" $ do
    let pts = [Point 0 0, Point 1 1, Point 2 2, Point 3 3]
        rps = runStream (cfgLinear 0.7) pts
    take 5 (map rpX rps) `shouldSatisfy` approxList [0.0, 0.7, 1.4, 2.1, 2.8]

  it "Merges multiple algorithms in ascending x; stable tie-break by algorithm order" $ do
    let cfg = cfgBoth 0.5
        pts = [Point 0 0, Point 1 1, Point 2 2, Point 3 3]
        rps = runStream cfg pts
        xs = map rpX rps
    xs `shouldSatisfy` (\ys -> and (zipWith (<=) ys (drop 1 ys)))
    -- Optional: when same x occurs, LinearTag should come before NewtonTag:
    let sameXPairs =
          [(a, b) | (a, b) <- zip rps (drop 1 rps), rpX a == rpX b]
    map (bimap rpAlgo rpAlgo) sameXPairs
      `shouldSatisfy` all (\(t1, t2) -> t1 == LinearTag && t2 == NewtonTag)

  it "Property: Newton n=3 is exact on any quadratic (streaming + flush)" $
    property $ \(Finite c0) (Finite c1) (Finite c2) ->
      let xs = [-2, -1, 0, 1, 2, 3] :: [Double]
          pts = [Point x (c0 + c1 * x + c2 * x * x) | x <- xs]
          rps = runStream (cfgNewton3 0.5) pts
          ok =
            [ approx (c0 + c1 * x + c2 * x * x) y
            | ResultPoint tag x y <- rps
            , tag == NewtonTag
            ]
       in and ok

newtype Finite = Finite Double deriving (Show)
instance Arbitrary Finite where
  arbitrary = Finite <$> suchThat arbitrary (\d -> not (isNaN d) && not (isInfinite d))
