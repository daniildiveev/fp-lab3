module Main where

import CLI (parseArgs)
import Engine.Stream (EngineState, flushEngine, initEngineState, stepEngine)
import Format.Printer (renderResult)
import Parse (parseLine)
import Types (
  Config (..),
  Delimiter (..),
  ParseError (..),
  Precision (..),
  SamplerConfig (..),
  ValidationError (..),
  X,
 )
import Util.Validate (stepValidate)

import Data.Maybe (fromMaybe)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdin, stdout, isEOF)

main :: IO ()
main = do
  cfg <- parseArgs
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  let delim = delimiter cfg
      prec = fromMaybe (Precision 6) (precision (sampler cfg))
      st0 = initEngineState cfg
  loop cfg delim prec st0 Nothing 1

loop :: Config -> Delimiter -> Precision -> EngineState -> Maybe X -> Int -> IO ()
loop cfg delim prec st lastX lnNo = do
  eof <- isEOF
  if eof
    then mapM_ (putStrLn . renderResult prec) (flushEngine cfg st)
    else do
      line <- getLine
      case parseLine delim line of
        Left (ParseError _ msg) -> do
          hPutStrLn stderr ("parse error at line " <> show lnNo <> ": " <> msg)
          exitFailure
        Right pt ->
          case stepValidate lastX pt of
            Left err -> do
              hPutStrLn stderr (prettyValidateError lnNo err)
              exitFailure
            Right lastX' -> do
              let (st', outs) = stepEngine cfg st pt
              mapM_ (putStrLn . renderResult prec) outs
              loop cfg delim prec st' lastX' (lnNo + 1)

prettyValidateError :: Int -> ValidationError -> String
prettyValidateError i e =
  case e of
    DuplicateX x -> "monotonicity error at line " <> show i <> ": duplicate x = " <> show x
    NonMonotonicX a b -> "monotonicity error at line " <> show i <> ": non-increasing x: " <> show a <> " -> " <> show b
    GeneralValidationError msg -> "validation error: " <> msg
    BadStep s -> "validation error: bad step " <> show s
    BadWindowSize n -> "validation error: bad window size " <> show n
