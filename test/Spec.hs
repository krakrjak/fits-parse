{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

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
import qualified Data.Fits as Fits
import Data.List ( unfoldr )
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Control.Exception (Exception(displayException), throwIO)
import GHC.RTS.Flags (MiscFlags(numIoWorkerThreads))

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
    dataArray
    sampleSpiral
    sampleNSOHeaders
    sampleNSO

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

    it "should strip trailing spaces from strings" $ do
      res <- parse parseKeywordValue "INSTRUME= 'VISP    '"
      res @?= ("INSTRUME", String "VISP")

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
      res @?= ("XTENSION", String "IMAGE")


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

      m <- parse parseHeader $ keywords h
      Map.lookup "CAL_URL" m @?= Just (String "https://docs.dkist.nso.edu/projects/visp/en/v2.0.1/l0_to_l1_visp.html")


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
        Map.lookup "XTENSION" res @?= Just (String "IMAGE")


requiredHeaders :: Test ()
requiredHeaders = describe "required headers" $ do

    --   res <- parse parseSimple $ keywords ["SIMPLE=    T"]
    --   res @?= Primary

    it "should parse bitpix" $ do
      res <- parse parseBitPix $ keywords ["BITPIX = 16"]
      res @?= SixteenBitInt

    it "should parse NAxes" $ do
      res <- parse parseNaxes $ keywords ["NAXIS = 3", "NAXIS1=1", "NAXIS2=2", "NAXIS3=3"]
      res @?= NAxes [1,2,3]

    it "should parse size" $ do
      res <- parse parseSizeKeywords $ keywords ["BITPIX = -32", "NAXIS=2", "NAXIS1=10", "NAXIS2=20"]
      res.bitpix @?= ThirtyTwoBitFloat
      res.naxes @?= NAxes [10,20]

    it "should include required headers in the keywords" $ do
      let fakeData = "1234" -- Related to NAXIS!
      h <- parse parsePrimary $ keywords ["SIMPLE = T", "BITPIX = 8", "NAXIS=2", "NAXIS1=2", "NAXIS2=2", "TEST='hi'"] <> fakeData
      h.extension @?= Primary
      Map.size h.header.keywords @?= 5
      Fits.lookup "NAXIS" h.header @?= Just (Integer 2) 

    it "should parse full extension" $ do
      h <- parse parseBinTable $ keywords ["XTENSION= 'BINTABLE'", "BITPIX = -32", "NAXIS=0", "PCOUNT=0", "GCOUNT=1"]
      h.extension @?= BinTable { pCount = 0, heap = "" }

dataArray :: Test ()
dataArray = describe "data array" $ do
    it "should grab correct data array" $ do
      let fakeData = "1234" -- Related to NAXIS!
      h <- parse parsePrimary $ keywords ["SIMPLE = T", "BITPIX = 8", "NAXIS=2", "NAXIS1=2", "NAXIS2=2", "TEST='hi'"] <> "       " <> fakeData
      h.dataArray @?= fakeData


sampleSpiral :: Test ()
sampleSpiral =
  describe "Spiral Sample FITS Parse" $ do
    it "should parse" $ do
      let fileSizeOnDisk = 1545444
      bs <- BS.readFile "./fits_files/Spiral_2_30_0_300_10_0_NoGrad.fits"
      hdu <- eitherFail $ getOneHDU bs
      -- hdu.header.size.bitpix @?= ThirtyTwoBitFloat
      -- hdu.header.size.naxes @?= NAxes [621, 621]
      --
      Fits.lookup "NAXIS" hdu.header @?= Just (Integer 2) 

      let payloadSize = BS.length hdu.dataArray

      -- Make sure we took the right number of bytes out of the file
      payloadSize @?= fileSizeOnDisk - hduBlockSize


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
      let ts = filter (not . ignore) $ T.lines $ TE.decodeUtf8 bs

      it "should parse all keywords individually" $ do
        forM_ ( zip [1..] ts ) $ \(n, t) -> do
          _ <- parse parseRecordLine $ keywords [TE.encodeUtf8 t]
          pure ()

      it "should parse NAxes correctly" $ do
        (sz, _) <- parse parseBinTableKeywords $ mconcat $ C8.lines bs
        sz.naxes @?= NAxes [32, 998]

  where
    ignore t = T.isPrefixOf "CONTINUE" t || T.isPrefixOf "END" t



sampleNSO :: Test ()
sampleNSO = do
  describe "NSO Sample FITS Parse" $ do
    bs <- liftIO $ BS.readFile "./fits_files/nso_dkist.fits"
    it "should parse empty primary header" $ do
      h0 <- eitherFail $ getOneHDU bs
      -- first header doesn't have any data
      BS.length h0.dataArray @?= 0


    it "should parse both HDUs" $ do
      hdus <- eitherFail $ getAllHDUs bs
      length hdus @?= 2

      [_, h2] <- pure hdus

      -- print h2.header
      Fits.lookup "INSTRUME" h2.header @?= Just (String "VISP")
      Fits.lookup "NAXIS" h2.header @?= Just (Integer 2)

      let sizeOnDisk = 161280
          countedHeaderBlocks = 11
          payloadLength = BS.length h2.dataArray


      print payloadLength
      print h2.extension

      -- h2.header.size.bitpix @?= EightBitInt
      -- h2.header.size.naxes @?= NAxes [32, 998]

      -- Payload size is as expected
      payloadLength @?= 32 * 998 * fromIntegral (bitPixToByteSize EightBitInt)
      pCount h2.extension @?= 95968

      -- TODO: we aren't getting the data correctly
      payloadLength @?= sizeOnDisk - (countedHeaderBlocks * hduBlockSize) - pCount h2.extension

      where
        pCount (BinTable { pCount = p}) = p
        pCount _ = 0





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



test :: IO ()
test = do
    bs <- BS.readFile "./fits_files/nso_dkist_headers.txt"
    h <- parse parseHeader bs
    print h





