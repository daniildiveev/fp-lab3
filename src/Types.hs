module Types (
  X,
  Y,
  Point (..),
  Interval (..),
  Delimiter (..),
  AlgorithmTag (..),
  LinearConfig (..),
  WindowSize (..),
  NewtonConfig (..),
  StartMode (..),
  Step (..),
  Precision (..),
  SamplerConfig (..),
  Algorithm (..),
  Config (..),
  -- ошибки
  ParseError (..),
  ValidationError (..),
  EngineError (..),
  -- выходные структуры
  ResultPoint (..),
  Window (..),
) where

type X = Double
type Y = Double

data Point = Point
  { px :: X
  , py :: Y
  }
  deriving (Show, Eq)

data Interval = Interval
  { iLeft :: X
  , iRight :: X
  }
  deriving (Show, Eq)

data Delimiter
  = Semicolon
  | Tab
  | Space
  | Comma
  | Auto
  deriving (Show, Eq)

data AlgorithmTag
  = LinearTag
  | NewtonTag
  deriving (Show, Eq, Ord, Enum, Bounded)

data LinearConfig = LinearConfig
  deriving (Show, Eq)

newtype WindowSize = WindowSize {unWindowSize :: Int}
  deriving (Show, Eq, Ord)

newtype NewtonConfig = NewtonConfig
  { windowSize :: WindowSize
  }
  deriving (Show, Eq)

data StartMode = AlignFirst | PassThrough
  deriving (Show, Eq)

newtype Step = Step {unStep :: Double}
  deriving (Show, Eq, Ord)

newtype Precision = Precision {unPrecision :: Int}
  deriving (Show, Eq, Ord)

data SamplerConfig = SamplerConfig
  { step :: Step
  , startMode :: StartMode
  , precision :: Maybe Precision
  }
  deriving (Show, Eq)

data Algorithm
  = AlgorithmLinear LinearConfig
  | AlgorithmNewton NewtonConfig
  deriving (Show, Eq)

data Config = Config
  { algorithms :: [Algorithm]
  , delimiter :: Delimiter
  , sampler :: SamplerConfig
  }
  deriving (Show, Eq)

data ParseError = ParseError
  { peLine :: Int
  , peMessage :: String
  }
  deriving (Show, Eq)

data ValidationError
  = NonMonotonicX X X
  | DuplicateX X
  | BadStep Double
  | BadWindowSize Int
  | GeneralValidationError String
  deriving (Show, Eq)

newtype EngineError = EngineError String
  deriving (Show, Eq)

data ResultPoint = ResultPoint
  { rpAlgo :: AlgorithmTag
  , rpX :: X
  , rpY :: Y
  }
  deriving (Show, Eq)

newtype Window = Window {unWindow :: [Point]}
  deriving (Show, Eq)
