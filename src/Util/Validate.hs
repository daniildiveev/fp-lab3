module Util.Validate
  ( validateMonotonic
  , validateConfig
  ) where

import Types (Config, Point, ValidationError)

validateMonotonic :: [Point] -> Either ValidationError [Point]
validateMonotonic = undefined

validateConfig :: Config -> Either ValidationError Config
validateConfig = undefined
