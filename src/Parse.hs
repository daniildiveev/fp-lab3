module Parse (
  autodetectDelimiter,
  parseLine,
  streamPoints,
) where

import Data.List.Split (splitOn)
import Types (Delimiter, ParseError, Point)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
 where
  dropWhileEnd p = reverse . dropWhile p . reverse

autodetectDelimiter :: String -> Maybe Delimiter
autodetectDelimiter s0 =
  let s = trim s0
   in if ';' `elem` s
        then Just Semicolon
        else
          if '\t' `elem` s
            then Just Tab
            else
              if ',' `elem` s
                then Just Comma
                else
                  if length (words s) == 2
                    then Just Space
                    else Nothing

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

parseLine :: Delimiter -> String -> Either ParseError Point
parseLine d0 s0 = do
  let s = trim s0
  d <- case d0 of
    Auto ->
      maybeToEither (ParseError 0 "cannot autodetect delimiter") (autodetectDelimiter s)
    _ -> Right d0

  let parts = case d of
        Semicolon -> splitOn ";" s
        Tab -> splitOn "\t" s
        Comma -> splitOn "," s
        Space -> words s -- robust to multiple spaces
        Auto -> words s -- unreachable after the case above, but keeps totality
  case parts of
    [sx, sy] -> do
      x <- maybeToEither (ParseError 0 ("bad X: " <> sx)) (readMaybe sx)
      y <- maybeToEither (ParseError 0 ("bad Y: " <> sy)) (readMaybe sy)
      Right (Point x y)
    _ -> Left (ParseError 0 "expected exactly two fields")

streamPoints :: Delimiter -> [String] -> Either ParseError [Point]
streamPoints d =
  traverse (\(i, ln) -> first (\(ParseError _ msg) -> ParseError i msg) (parseLine d ln))
    . zip [1 ..]
