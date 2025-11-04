module Engine.Stream (
  EngineState,
  initEngineState,
  stepEngine,
  flushEngine,
) where

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

data AlgoState = AlgoState
  { asTag :: AlgorithmTag
  , asK :: Int
  , asSafe :: Window -> Either EngineError Interval
  , asEval :: Window -> X -> Either EngineError Y
  , asResampler :: ResamplerState
  , asWindow :: [Point]
  , asFirstWindow :: Maybe Window
  , asLastWindow :: Maybe Window
  }

data EngineState = EngineState
  { esAlgos :: [AlgoState]
  , esOrder :: M.Map AlgorithmTag Int
  }

initEngineState :: Config -> EngineState
initEngineState cfg =
  EngineState
    { esAlgos = map (mkAlgo (sampler cfg)) (algorithms cfg)
    , esOrder = orderMap (algorithms cfg)
    }
 where
  mkAlgo :: SamplerConfig -> Algorithm -> AlgoState
  mkAlgo smp alg =
    case alg of
      AlgorithmLinear lc ->
        AlgoState
          LinearTag
          (linearRequiredWindowSize lc)
          (linearSafeInterval lc)
          (linearEvalAt lc)
          rs0
          []
          Nothing
          Nothing
      AlgorithmNewton nc ->
        AlgoState
          NewtonTag
          (newtonRequiredWindowSize nc)
          (newtonSafeInterval nc)
          (newtonEvalAt nc)
          rs0
          []
          Nothing
          Nothing
   where
    rs0 = ResamplerState {rsStep = step smp, rsStart = startMode smp, rsCursor = Nothing}

stepEngine :: Config -> EngineState -> Point -> (EngineState, [ResultPoint])
stepEngine _ st p =
  let (algos', chunks) = unzip (map (stepAlgo p) (esAlgos st))
      merged = foldl (mergeBy (esOrder st)) [] chunks
   in (st {esAlgos = algos'}, merged)

flushEngine :: Config -> EngineState -> [ResultPoint]
flushEngine _ st =
  let outs = map flushAlgo (esAlgos st)
   in foldl (mergeBy (esOrder st)) [] outs

stepAlgo :: Point -> AlgoState -> (AlgoState, [ResultPoint])
stepAlgo p as =
  let k = asK as
      win0 = asWindow as
      win1 = if length win0 >= k then drop 1 (win0 ++ [p]) else win0 ++ [p]
   in if length win1 < k
        then (as {asWindow = win1}, [])
        else
          let w = Window win1
              firstW = case asFirstWindow as of
                Nothing -> Just w
                j -> j
           in case asSafe as w of
                Left _err ->
                  ( as
                      { asWindow = win1
                      , asFirstWindow = firstW
                      , asLastWindow = Just w
                      }
                  , []
                  )
                Right iv ->
                  let (xs, rs') = resample (asResampler as) iv
                      ysE = map (asEval as w) xs
                      chunk =
                        [ ResultPoint (asTag as) x y
                        | (x, Right y) <- zip xs ysE
                        ]
                   in ( as
                          { asWindow = win1
                          , asResampler = rs'
                          , asFirstWindow = firstW
                          , asLastWindow = Just w
                          }
                      , chunk
                      )

flushAlgo :: AlgoState -> [ResultPoint]
flushAlgo as =
  case asLastWindow as of
    Nothing -> []
    Just w ->
      case asSafe as w of
        Left _err -> []
        Right iv ->
          let (xs, _rs') = resample (asResampler as) iv
              ysE = map (asEval as w) xs
           in [ ResultPoint (asTag as) x y
              | (x, Right y) <- zip xs ysE
              ]

orderMap :: [Algorithm] -> M.Map AlgorithmTag Int
orderMap algs = M.fromList (zip (map algoTag algs) [0 ..])

algoTag :: Algorithm -> AlgorithmTag
algoTag a =
  case a of
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
