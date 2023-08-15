{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

import Data.Text ( Text )
import Data.ByteString ( ByteString )
import Control.Monad.Writer
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Fits.MegaParser
import Data.Fits
import Data.List ( unfoldr )
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Control.Exception (Exception(displayException))

main :: IO ()
main =
  testMain $ runTests "Tests" $ do
    basicParsing
    keywordValueLines
    comments
    continue
    fullRecord
    fullRecordLine
    headerMap
    requiredHeaders
    sampleSpiral
    sampleNSOHeaders
    -- sampleNSO

parse :: Parser a -> ByteString -> IO a
parse p inp =
    case M.parse p "Test" inp of
        Left e -> fail $ displayException e
        Right v -> pure v

eitherFail :: Show err => Either err a -> IO a
eitherFail (Left e) = fail (show e)
eitherFail (Right a) = return a

keywords :: [ByteString] -> ByteString
keywords ts = mconcat (map pad ts) <> "END"

pad :: ByteString -> ByteString
pad m =
  let n = 80 - BS.length m
  in m <> C8.replicate n ' '


basicParsing :: Test ()
basicParsing = describe "Basic Parsing" $ do
  it "should parse a string" $ do
    res <- parse parseStringValue "'hello there' "
    res @?= "hello there"

  it "should parse a number value" $ do
    res <- parse parseValue "42 "
    res @?= Integer 42

  it "should parse a keyword" $ do
    res <- parse parseKeyword "WS_TEMP ="
    res @?= Keyword "WS_TEMP"

  it "should handle keyword symbols" $ do
    res <- parse parseKeyword "OBSGEO-X=   -5466045.256954942 / [m]"
    res @?= Keyword "OBSGEO-X"



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

    it "should handle keyword symbols" $ do
      res <- parse parseKeywordRecord $ keywords ["OBSGEO-X=   -5466045.256954942 / [m]"]
      res @?= ("OBSGEO-X", Float (-5466045.256954942))

    it "should handle extension" $ do
      res <- parse parseKeywordRecord $ keywords ["XTENSION= 'IMAGE   '"]
      res @?= ("XTENSION", String "IMAGE   ")


fullRecordLine :: Test ()
fullRecordLine = describe "parseRecordLine" $ do
    it "should parse a normal line" $ do
      res <- parse parseRecordLine "NAXIS1  =                  100 / [pix]                                          END"
      res @?= Just ("NAXIS1", Integer 100)

    it "should parse a comment line" $ do
      res <- parse parseRecordLine "COMMENT ------------------------------ Telescope -------------------------------END"
      res @?= Nothing

    it "should parse a blank line" $ do
      res <- parse parseRecordLine $ keywords [" "]
      res @?= Nothing


comments :: Test ()
comments = describe "Full-line comments" $ do
    it "should parse full-line comments" $ do
      res <- parse parseLineComment $ keywords ["COMMENT --------------------------- VISP Instrument ----------------------------"]
      res @?= Comment "--------------------------- VISP Instrument ----------------------------"

    it "should parse comments with text" $ do
      res <- parse parseLineComment $ keywords ["COMMENT  Keys describing the pointing and operation of the telescope. Including "]
      res @?= Comment " Keys describing the pointing and operation of the telescope. Including "

    it "should parse blank comments" $ do
      res <- parse parseLineComment $ keywords ["COMMENT                                                                         "]
      res @?= Comment "                                                                        "


continue :: Test ()
continue = describe "Continue Keyword" $ do

    it "should be picked up in parseValue" $ do
      res <- parse parseValue $ keywords ["'hello&'CONTINUE '!'"]
      res @?= String "hello!"


    it "should combine continue into previous keyword" $ do
      let h = [ "CAL_URL = 'https://docs.dkist.nso.edu/projects/visp/en/v2.0.1/l0_to_l1_visp.ht&'"
              , "CONTINUE  'ml'                                                                  "
              ]

      res <- parse parseHeader $ keywords h
      Map.lookup "CAL_URL" res @?= Just (String "https://docs.dkist.nso.edu/projects/visp/en/v2.0.1/l0_to_l1_visp.html")


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

    it "should handle xtension" $ do
        res <- parse parseHeader $ keywords ["XTENSION= 'IMAGE   '"]
        Map.size res @?= 1
        Map.lookup "XTENSION" res @?= Just (String "IMAGE   ")


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
      hdu <- eitherFail $ getOneHDU bs
      hdu.size.bitpix @?= ThirtyTwoBitFloat
      hdu.size.naxes @?= NAxes [621, 621]
      -- TODO: what should the length be?
      -- BS.length (payloadData hdu) @?= 32*621*621
      -- print (BS.length (payloadData hdu))
      -- error "NOPE"
      return ()


sampleNSOHeaders :: Test ()
sampleNSOHeaders = do
  describe "NSO Stripped Headers" $ do

    it "should parse comment block" $ do
      let h = [ "DATASUM = '550335088'          / data unit checksum updated 2023-04-22T04:10:59 " 
              , "   "
              , "COMMENT ------------------------------ Telescope -------------------------------"
              , "COMMENT  Keys describing the pointing and operation of the telescope. Including "
              , "COMMENT     the FITS WCS keys describing the world coordinates of the array.    "
              ]
      m <- parse parseHeader $ keywords h
      Map.size m @?= 1


    describe "sample header file" $ do
      bs <- liftIO $ BS.readFile "./fits_files/nso_dkist_headers.txt"
      let ts = filter notContinue $ T.lines $ TE.decodeUtf8 bs

      forM_ ( zip [1..] ts ) $ \(n, t) -> do
        it ("nso_dkist_headers[" <> show n <> "]") $ do
          m <- parse parseHeader $ TE.encodeUtf8 $ t <> "END"
          pure ()
  where
    notContinue = not . T.isPrefixOf "CONTINUE"


test :: IO ()
test = do
    bs <- liftIO $ BS.readFile "./fits_files/nso_dkist_headers.txt"
    let ts = T.lines $ TE.decodeUtf8 bs

    forM_ ts $ \t -> do
      putStrLn $ T.unpack t
      m <- parse parseHeader $ TE.encodeUtf8 $ t <> "END"
      pure ()
        -- print m


sampleNSO :: Test ()
sampleNSO = do
  describe "NSO Sample FITS Parse" $ do
    it "should parse" $ do
      bs <- BS.readFile "./fits_files/nso_dkist.fits"
      hdus <- eitherFail $ getAllHDUs bs
      length hdus @?= 2



-- Test monad with describe/it
newtype Test a = Test {runTest :: WriterT [TestTree] IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadWriter [TestTree])

runTests :: TestName -> Test () -> IO TestTree
runTests n (Test t) = do
    tests <- execWriterT t :: IO [TestTree]
    return $ testGroup n tests


-- testMain t = do
--     ts <-

describe :: TestName -> Test () -> Test ()
describe n t = do
    ts <- liftIO $ runTests n t
    tell [ts]

it :: TestName -> IO () -> Test ()
it n a = do
    tell [testCase n a]


testMain :: IO TestTree -> IO ()
testMain mtt = do
    tt <- mtt
    defaultMain tt
