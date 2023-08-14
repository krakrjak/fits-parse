{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Control.Monad.Writer
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Fits.MegaParser
import Data.Fits
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

main :: IO ()
main =
  defaultMain $ runTests "Tests" $ do
    keywordValueLines
    -- basicParsing
    -- sampleSpiral
    -- sampleNSO
    --
    --
parse :: Parser a -> Text -> Either ParseErr a
parse p = M.parse p "Test"

keywordValueLines :: Test ()
keywordValueLines = describe "parse keyword=value" $ do
    it "should parse an integer" $ do
        parse parseKeywordValue "key=42" @?= Right ("key", Integer 42)

    it "should parse a string" $ do
        parse parseKeywordValue "key='value'" @?= Right ("key", String "value")

    it "should absorb spaces" $ do
        parse parseKeywordValue "key   = 'value'   " @?= Right ("key", String "value")

    it "should parse a float" $ do
        parse parseKeywordValue "key =   44.88 " @?= Right ("key", Float 44.88)

    it "should parse a negative number" $ do
        parse parseKeywordValue "key = -44.88" @?= Right ("key", Float ( -44.88 ))

headerMap :: Test ()
headerMap = describe "parse all headers" $ do
    it "should parse a list of headers" $ do
        let res = parse parseRawHeader "key1='value'\nkey2=42"
        case res of
            Left e -> fail (show e)
            Right hs -> do
                (Map.lookup "key1" hs) @?= Just (String "value")
                (Map.lookup "key2" hs) @?= Just (Integer 42)

    

        

basicParsing :: Test ()
basicParsing = describe "Basic Parsing" $ do
  it "should parse simple bzero header" $ do
    let res = M.runParser parseBzero "Test" "bzero=3"
    res @?= Right 3

sampleSpiral :: Test ()
sampleSpiral =
  describe "Spiral Sample FITS Parse" $ do
    it "should parse" $ do
      bs <- BS.readFile "./fits_files/Spiral_2_30_0_300_10_0_NoGrad.fits"
      hdus <- getAllHDUs bs
      length hdus @?= 1

sampleNSO :: Test ()
sampleNSO = do
  describe "NSO Sample FITS Parse" $ do
    it "should parse" $ do
      bs <- BS.readFile "./fits_files/nso_dkist.fits"
      hdus <- getAllHDUs bs
      length hdus @?= 1

-- Test monad with describe/it
newtype Test a = Test {runTest :: Writer [TestTree] a}
  deriving (Functor, Applicative, Monad, MonadWriter [TestTree])

runTests :: TestName -> Test () -> TestTree
runTests n (Test t) =
  let tests = execWriter t :: [TestTree]
   in testGroup n tests

describe :: TestName -> Test () -> Test ()
describe n t =
  tell [runTests n t]

it :: TestName -> IO () -> Test ()
it n a = do
  tell [testCase n a]
