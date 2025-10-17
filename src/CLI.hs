module CLI (parseArgs) where

import Types (
  Algorithm (..),
  Config (..),
  Delimiter (..),
  LinearConfig (..),
  NewtonConfig (..),
  Precision (..),
  SamplerConfig (..),
  StartMode (..),
  Step (..),
  ValidationError (..),
  WindowSize (..),
 )
import Util.Validate (validateConfig)

import Options.Applicative (
  Parser,
  auto,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  optional,
  short,
  strOption,
  switch,
  value,
  (<**>),
 )
import System.Exit (die)

parseArgs :: IO Config
parseArgs = do
  cfg <-
    execParser
      ( info
          (configP <**> helper)
          (fullDesc <> header "fp-lab3 — streaming interpolation CLI")
      )
  case validateConfig cfg of
    Left (GeneralValidationError msg) -> die msg
    Left (BadStep s) -> die ("invalid step: " <> show s)
    Left (BadWindowSize n) -> die ("invalid Newton window n: " <> show n)
    Left _ -> die "invalid configuration"
    Right ok -> pure ok

configP :: Parser Config
configP =
  Config
    <$> algosP
    <*> delimP
    <*> samplerP

algosP :: Parser [Algorithm]
algosP =
  combine
    <$> switch (long "linear" <> help "Enable linear interpolation")
    <*> optional
      ( option
          auto
          ( long "n"
              <> short 'n'
              <> metavar "N"
              <> help "Newton window size (enables Newton)"
          )
      )
 where
  combine useLinear mN =
    [AlgorithmLinear LinearConfig | useLinear]
      ++ maybe [] (\n -> [AlgorithmNewton (NewtonConfig (WindowSize n))]) mN

samplerP :: Parser SamplerConfig
samplerP =
  SamplerConfig . Step
    <$> option auto (long "step" <> metavar "DOUBLE" <> help "Sampling step")
    <*> startModeP
    <*> optional
      ( Precision
          <$> option
            auto
            ( long "precision"
                <> metavar "INT"
                <> help "Decimal precision for output"
            )
      )

startModeP :: Parser StartMode
startModeP =
  toMode
    <$> strOption
      ( long "start-mode"
          <> metavar "align-first|pass-through"
          <> value "align-first"
          <> help "Grid alignment strategy"
      )
 where
  toMode s = case s of
    "align-first" -> AlignFirst
    "pass-through" -> PassThrough
    _ -> AlignFirst

delimP :: Parser Delimiter
delimP =
  toDelim
    <$> strOption
      ( long "delim"
          <> metavar "auto|semicolon|tab|space"
          <> value "auto"
          <> help "Input delimiter"
      )
 where
  toDelim s = case s of
    "auto" -> Auto
    "semicolon" -> Semicolon
    "tab" -> Tab
    "space" -> Space
    _ -> Auto
