module Engine (runEngine) where

import qualified Data.Map.Strict as M
import Interpolation.Linear (
  linearEvalAt,
  linearRequiredWindowSize,
  linearSafeInterval,
 )
import Interpolation.Newton (
  newtonEvalAt,
  newtonRequiredWindowSize,
  newtonSafeInterval,
 )
import Stream.Resampler (ResamplerState (..), resample)
import Stream.SlidingWindow (slidingWindows)
import Types (
  Algorithm (..),
  AlgorithmTag (..),
  Config (..),
  EngineError (..),
  Interval (..),
  Point (..),
  ResultPoint (..),
  SamplerConfig (..),
  Window (..),
  X,
  Y,
 )

runEngine :: Config -> [Point] -> Either EngineError [ResultPoint]
runEngine cfg pts =
  case algorithms cfg of
    [] -> Left (EngineError "No algorithms selected")
    algs ->
      let perAlgo = map (produceFor (sampler cfg) pts) algs
          ordMap = orderMap algs
          out = foldl1 (mergeBy ordMap) perAlgo
       in Right out

produceFor :: SamplerConfig -> [Point] -> Algorithm -> [ResultPoint]
produceFor smp points alg =
  case alg of
    AlgorithmLinear lc ->
      build
        (linearRequiredWindowSize lc)
        (linearSafeInterval lc) -- Window -> Either EngineError Interval
        (linearEvalAt lc) -- Window -> X -> Either EngineError Y
        LinearTag
    AlgorithmNewton nc ->
      build
        (newtonRequiredWindowSize nc)
        (newtonSafeInterval nc)
        (newtonEvalAt nc)
        NewtonTag
 where
  build ::
    Int ->
    (Window -> Either EngineError Interval) ->
    (Window -> X -> Either EngineError Y) ->
    AlgorithmTag ->
    [ResultPoint]
  build k safeF evalF tag =
    let ws = slidingWindows k points
        st0 =
          ResamplerState
            { rsStep = step smp
            , rsStart = startMode smp
            , rsCursor = Nothing
            }
     in produceStream safeF evalF tag st0 ws

  produceStream ::
    (Window -> Either EngineError Interval) ->
    (Window -> X -> Either EngineError Y) ->
    AlgorithmTag ->
    ResamplerState ->
    [Window] ->
    [ResultPoint]
  produceStream _ _ _ _ [] = []
  produceStream safeF evalF tag st (w : ws') =
    case safeF w of
      Left _err ->
        -- Bad window: skip it, do not advance resampler state
        produceStream safeF evalF tag st ws'
      Right iv ->
        let (xs, st') = resample st iv
            ysE = map (evalF w) xs
            chunk =
              [ ResultPoint tag x y
              | (x, Right y) <- zip xs ysE
              ]
         in chunk ++ produceStream safeF evalF tag st' ws'

orderMap :: [Algorithm] -> M.Map AlgorithmTag Int
orderMap algs = M.fromList (zip (map algoTag algs) [0 ..])

algoTag :: Algorithm -> AlgorithmTag
algoTag a = case a of
  AlgorithmLinear _ -> LinearTag
  AlgorithmNewton _ -> NewtonTag

mergeBy :: M.Map AlgorithmTag Int -> [ResultPoint] -> [ResultPoint] -> [ResultPoint]
mergeBy ord = go
 where
  prio t = M.findWithDefault maxBound t ord
  go as bs =
    case (as, bs) of
      ([], _) -> bs
      (_, []) -> as
      (a : as', b : bs') ->
        case compare (rpX a) (rpX b) of
          LT -> a : go as' (b : bs')
          GT -> b : go (a : as') bs'
          EQ ->
            if prio (rpAlgo a) <= prio (rpAlgo b)
              then a : go as' (b : bs')
              else b : go (a : as') bs'
