module Parse
  ( autodetectDelimiter
  , parseLine
  , streamPoints
  ) where

import Types (Delimiter, ParseError, Point)

autodetectDelimiter :: String -> Maybe Delimiter
autodetectDelimiter = undefined

parseLine :: Delimiter -> String -> Either ParseError Point
parseLine = undefined

streamPoints :: Delimiter -> [String] -> Either ParseError [Point]
streamPoints = undefined
