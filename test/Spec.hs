{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Control.Monad.Writer
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Fits.MegaParser
import Data.Fits
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Control.Exception (Exception(displayException))

main :: IO ()
main =
  defaultMain $ runTests "Tests" $ do
    basicParsing
    keywordValueLines
    fullRecord
    headerMap
    requiredHeaders
    sampleSpiral
    sampleNSO

parse :: Parser a -> Text -> IO a
parse p inp =
    case M.parse p "Test" inp of
        Left e -> fail $ displayException e
        Right v -> pure v

keywords :: [Text] -> Text
keywords ts = mconcat (map pad ts) <> "END"

pad :: Text -> Text
pad m =
  let n = 80 - T.length m
  in m <> T.replicate n " "


basicParsing :: Test ()
basicParsing = describe "Basic Parsing" $ do
  it "should parse a string" $ do
    res <- parse parseStringValue "'hello there' "
    res @?= "hello there"

  it "should parse a number value" $ do
    res <- parse parseValue "42 "
    res @?= Integer 42



keywordValueLines :: Test ()
keywordValueLines = describe "parse keyword=value" $ do
    it "should parse an integer" $ do
        res <- parse parseKeywordValue "KEY=42 "
        res @?= ("KEY", Integer 42)

    it "should parse a string" $ do
        res <- parse parseKeywordValue "KEY='value'" 
        res @?= ("KEY", String "value")

    it "should absorb spaces" $ do
        res <- parse parseKeywordValue "KEY   = 'value'   "
        res @?= ("KEY", String "value")

    it "should parse a float" $ do
        res <- parse parseKeywordValue "KEY =   44.88 "
        res @?= ("KEY", Float 44.88)

    it "should parse a negative number" $ do
        res <- parse parseKeywordValue "KEY = -44.88"
        res @?= ("KEY", Float ( -44.88 ))

    it "should parse a logical constant" $ do
        res <- parse parseKeywordValue "KEY=     T " 
        res @?= ("KEY", Logic T)

    it "should ignore comments" $ do
        res <- parse parseKeywordValue "SIMPLE  =                    T / conforms to FITS standard" 
        res @?= ("SIMPLE", Logic T)

fullRecord :: Test ()
fullRecord = describe "parseKeywordRecord" $ do
  it "should parse an 80 character record" $ do
    res <- parse parseKeywordRecord (keywords ["KEYWORD = 12345"])
    res @?= ("KEYWORD", Integer 12345)

  it "should parse an a record and comment" $ do
    res <- parse parseKeywordRecord (keywords ["KEYWORD = 12345 / this is a comment"])
    res @?= ("KEYWORD", Integer 12345)

  it "should parse a record, comment, followed by next keyword" $ do
      res <- parse parseKeywordRecord $ keywords ["SIMPLE  =                    T / conforms to FITS standard"]
      res @?= ("SIMPLE", Logic T)



headerMap :: Test ()
headerMap = describe "full header" $ do
    it "should parse single header" $ do
        res <- parse parseHeader $ keywords ["KEY1='value'"]
        Map.size res @?= 1
        Map.lookup "KEY1" res @?= Just (String "value")

    it "should parse multiple headers " $ do
        res <- parse parseHeader $ keywords ["KEY1='value'", "KEY2=  23"]
        Map.size res @?= 2
        Map.lookup "KEY2" res @?= Just (Integer 23)

    it "should ignore comments" $ do
        res <- parse parseHeader $ keywords ["KEY1='value' / this is a comment"]
        Map.size res @?= 1
        Map.lookup "KEY1" res @?= Just (String "value")


requiredHeaders :: Test ()
requiredHeaders = describe "required headers" $ do
    it "should parse simple format" $ do
      res <- parse (parseSimple =<< parseHeader) $ keywords ["SIMPLE=    T"]
      res @?= Conformant

    it "should parse bitpix" $ do
      res <- parse (parseBitPix =<< parseHeader) $ keywords ["BITPIX = 16"]
      res @?= SixteenBitInt

    it "should parse NAxes" $ do
      res <- parse (parseNaxes =<< parseHeader) $ keywords ["NAXIS = 3", "NAXIS1=1", "NAXIS2=2", "NAXIS3=3"]
      res @?= NAxes [1,2,3]

    it "should parse size" $ do
      res <- parse (parseSizeKeywords =<< parseHeader) $ keywords ["NAXIS=2", "NAXIS1=10", "NAXIS2=20", "BITPIX = -32"]
      res.bitpix @?= ThirtyTwoBitFloat
      res.naxes @?= NAxes [10,20]


sampleSpiral :: Test ()
sampleSpiral =
  describe "Spiral Sample FITS Parse" $ do
    it "should parse" $ do
      bs <- BS.readFile "./fits_files/Spiral_2_30_0_300_10_0_NoGrad.fits"
      (hdu, _) <- getOneHDU bs
      hdu.size.bitpix @?= ThirtyTwoBitFloat
      hdu.size.naxes @?= NAxes [621, 621]
      -- TODO: what should the length be?
      -- BS.length (payloadData hdu) @?= 32*621*621
      -- print (BS.length (payloadData hdu))
      -- error "NOPE"
      return ()



sampleNSO :: Test ()
sampleNSO = do
  describe "NSO Sample FITS Parse" $ do
    it "should parse" $ do
      bs <- BS.readFile "./fits_files/nso_dkist.fits"
      x <- getOneHDU bs
      print "OK"

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
