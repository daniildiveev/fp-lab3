module ParserSpec (spec) where

import Parse (
  autodetectDelimiter,
  parseLine,
  streamPoints,
 )
import Test.Hspec
import Test.QuickCheck
import Types (
  Delimiter (..),
  ParseError (..),
  Point (..),
 )

newtype FiniteD = FiniteD {unFinite :: Double} deriving (Show)

instance Arbitrary FiniteD where
  arbitrary = FiniteD <$> suchThat arbitrary (\d -> not (isNaN d) && not (isInfinite d))

-- make exhaustive even if your Delimiter has more constructors (e.g. Comma)
mkLine :: Delimiter -> Double -> Double -> String
mkLine Semicolon x y = show x <> ";" <> show y
mkLine Tab x y = show x <> "\t" <> show y
mkLine Space x y = show x <> " " <> show y
mkLine Auto x y = show x <> " " <> show y
mkLine _ x y = show x <> " " <> show y -- default for any extra constructor

spec :: Spec
spec = describe "Parse" $ do
  describe "autodetectDelimiter" $ do
    it "detects Semicolon" $
      autodetectDelimiter "1.0;2.0" `shouldBe` Just Semicolon
    it "detects Tab" $
      autodetectDelimiter "1.0\t2.0" `shouldBe` Just Tab
    it "detects Space" $
      autodetectDelimiter "1.0 2.0" `shouldBe` Just Space
    it "returns Nothing for invalid" $
      autodetectDelimiter "1.0,2.0,3.0" `shouldBe` Nothing

  describe "parseLine (unit)" $ do
    it "parses semicolon pair" $
      parseLine Semicolon "3.5;4.2" `shouldBe` Right (Point 3.5 4.2)
    it "parses tab pair" $
      parseLine Tab "3.5\t4.2" `shouldBe` Right (Point 3.5 4.2)
    it "parses space pair" $
      parseLine Space "3.5 4.2" `shouldBe` Right (Point 3.5 4.2)
    it "auto-detects delimiter on the same line" $
      parseLine Auto "3.5 4.2" `shouldBe` Right (Point 3.5 4.2)

  describe "parseLine (property)" $ do
    it "roundtrips for Semicolon" $
      property $ \(FiniteD x) (FiniteD y) ->
        parseLine Semicolon (mkLine Semicolon x y) == Right (Point x y)
    it "roundtrips for Tab" $
      property $ \(FiniteD x) (FiniteD y) ->
        parseLine Tab (mkLine Tab x y) == Right (Point x y)
    it "roundtrips for Space" $
      property $ \(FiniteD x) (FiniteD y) ->
        parseLine Space (mkLine Space x y) == Right (Point x y)

  describe "streamPoints" $ do
    it "parses multiple lines and annotates errors with line numbers" $ do
      let ls = ["0 0", "1 1", "oops", "2 2"]
      case streamPoints Space ls of
        Left (ParseError i _msg) -> i `shouldBe` 3
        Right _ -> expectationFailure "expected parse error at line 3"
