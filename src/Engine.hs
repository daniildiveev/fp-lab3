module Engine (runEngine) where

import Types (Config, EngineError, Point, ResultPoint)

runEngine :: Config -> [Point] -> Either EngineError [ResultPoint]
runEngine = undefined
