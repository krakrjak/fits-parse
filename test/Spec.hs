{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
-- qualified imports
---- base
import qualified Data.Map.Strict as Map
---- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
---- megaparsec
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
---- text
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
---- local imports
import qualified Data.Fits as Fits
-- symbol based imports
---- base
import Control.Exception (Exception(displayException), throwIO)
import Control.Monad.Writer
import Data.List ( unfoldr )
---- bytestring
import Data.ByteString ( ByteString )
---- ghc
import GHC.RTS.Flags (MiscFlags(numIoWorkerThreads))
---- microlens
import Lens.Micro ((^.))
---- tasty
import Test.Tasty
import Test.Tasty.HUnit
---- text
import Data.Text ( Text )
---- local-imports
import Data.Fits ( Axes
                 , BitPixFormat(..)
                 , bitPixToByteSize
                 , Comment(Comment)
                 , Dimensions
                 , axes
                 , bitpix
                 , Extension(..)
                 , Header
                 , keywords
                 , HeaderDataUnit
                 , header
                 , dimensions
                 , extension
                 , mainData
                 , Keyword(Keyword)
                 , LogicalConstant(..)
                 , Value(..)
                 , hduBlockSize
                 )
import Data.Fits.MegaParser
import Data.Fits.Read

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

flattenKeywords :: [ByteString] -> ByteString
flattenKeywords ts = mconcat (map pad ts) <> "END"

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
      res <- parse parseKeywordRecord (flattenKeywords ["KEYWORD = 12345"])
      res @?= ("KEYWORD", Integer 12345)

    it "should parse an a record and comment" $ do
      res <- parse parseKeywordRecord (flattenKeywords ["KEYWORD = 12345 / this is a comment"])
      res @?= ("KEYWORD", Integer 12345)

    it "should parse a record, comment, followed by next keyword" $ do
      res <- parse parseKeywordRecord $ flattenKeywords ["SIMPLE  =                    T / conforms to FITS standard"]
      res @?= ("SIMPLE", Logic T)

    it "should handle keyword symbols" $ do
      res <- parse parseKeywordRecord $ flattenKeywords ["OBSGEO-X=   -5466045.256954942 / [m]"]
      res @?= ("OBSGEO-X", Float (-5466045.256954942))

    it "should handle extension" $ do
      res <- parse parseKeywordRecord $ flattenKeywords ["XTENSION= 'IMAGE   '"]
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
      res <- parse parseRecordLine $ flattenKeywords [" "]
      res @?= Nothing


comments :: Test ()
comments = describe "Full-line comments" $ do
    it "should parse full-line comments" $ do
      res <- parse parseLineComment $ flattenKeywords ["COMMENT --------------------------- VISP Instrument ----------------------------"]
      res @?= Comment "--------------------------- VISP Instrument ----------------------------"

    it "should parse comments with text" $ do
      res <- parse parseLineComment $ flattenKeywords ["COMMENT  Keys describing the pointing and operation of the telescope. Including "]
      res @?= Comment " Keys describing the pointing and operation of the telescope. Including "

    it "should parse blank comments" $ do
      res <- parse parseLineComment $ flattenKeywords ["COMMENT                                                                         "]
      res @?= Comment "                                                                        "


continue :: Test ()
continue = describe "Continue Keyword" $ do

    it "should be picked up in parseValue" $ do
      res <- parse parseValue $ flattenKeywords ["'hello&'CONTINUE '!'"]
      res @?= String "hello!"

    it "should combine continue into previous keyword" $ do
      let h = [ "CAL_URL = 'https://docs.dkist.nso.edu/projects/visp/en/v2.0.1/l0_to_l1_visp.ht&'"
              , "CONTINUE  'ml'                                                                  "
              ]

      m <- parse parseHeader $ flattenKeywords h
      Map.lookup "CAL_URL" m @?= Just (String "https://docs.dkist.nso.edu/projects/visp/en/v2.0.1/l0_to_l1_visp.html")


headerMap :: Test ()
headerMap = describe "full header" $ do
    it "should parse single header" $ do
        res <- parse parseHeader $ flattenKeywords ["KEY1='value'"]
        Map.size res @?= 1
        Map.lookup "KEY1" res @?= Just (String "value")

    it "should parse multiple headers " $ do
        res <- parse parseHeader $ flattenKeywords ["KEY1='value'", "KEY2=  23"]
        Map.size res @?= 2
        Map.lookup "KEY2" res @?= Just (Integer 23)

    it "should ignore comments" $ do
        res <- parse parseHeader $ flattenKeywords ["KEY1='value' / this is a comment"]
        Map.size res @?= 1
        Map.lookup "KEY1" res @?= Just (String "value")

    it "should handle xtension" $ do
        res <- parse parseHeader $ flattenKeywords ["XTENSION= 'IMAGE   '"]
        Map.size res @?= 1
        Map.lookup "XTENSION" res @?= Just (String "IMAGE")


requiredHeaders :: Test ()
requiredHeaders = describe "required headers" $ do

    --   res <- parse parseSimple $ keywords ["SIMPLE=    T"]
    --   res @?= Primary

    it "should parse bitpix" $ do
      res <- parse parseBitPix $ flattenKeywords ["BITPIX = 16"]
      res @?= SixteenBitInt

    it "should parse NAxes" $ do
      res <- parse parseNaxes $ flattenKeywords ["NAXIS = 3", "NAXIS1=1", "NAXIS2=2", "NAXIS3=3"]
      res @?= [1,2,3]

    it "should parse size" $ do
      res <- parse parseDimensions $ flattenKeywords ["BITPIX = -32", "NAXIS=2", "NAXIS1=10", "NAXIS2=20"]
      (res ^. bitpix) @?= ThirtyTwoBitFloat
      (res ^. axes) @?= [10,20]

    it "should include required headers in the keywords" $ do
      let fakeData = "1234" -- Related to NAXIS!
      h <- parse parsePrimary $ flattenKeywords ["SIMPLE = T", "BITPIX = 8", "NAXIS=2", "NAXIS1=2", "NAXIS2=2", "TEST='hi'"] <> fakeData
      (h ^. extension) @?= Primary
      Map.size (h ^. header . keywords) @?= 5
      Fits.lookup "NAXIS" (h ^. header) @?= Just (Integer 2) 

    it "should parse full extension" $ do
      h <- parse parseBinTable $ flattenKeywords ["XTENSION= 'BINTABLE'", "BITPIX = -32", "NAXIS=0", "PCOUNT=0", "GCOUNT=1"]
      (h ^. extension) @?= BinTable 0 ""

dataArray :: Test ()
dataArray = describe "data array" $ do
    it "should grab correct data array" $ do
      let fakeData = "1234" -- Related to NAXIS!
      h <- parse parsePrimary $ flattenKeywords ["SIMPLE = T", "BITPIX = 8", "NAXIS=2", "NAXIS1=2", "NAXIS2=2", "TEST='hi'"] <> "       " <> fakeData
      (h ^. mainData) @?= fakeData


sampleSpiral :: Test ()
sampleSpiral =
  describe "Spiral Sample FITS Parse" $ do
    it "should parse" $ do
      let fileSizeOnDisk = 1545444
      bs <- BS.readFile "./fits_files/Spiral_2_30_0_300_10_0_NoGrad.fits"
      hdu <- eitherFail $ readPrimaryHDU bs
      -- hdu.header.size.bitpix @?= ThirtyTwoBitFloat
      -- hdu.header.size.naxes @?= NAxes [621, 621]
      --
      Fits.lookup "NAXIS" (hdu ^. header) @?= Just (Integer 2) 

      let payloadSize = BS.length (hdu ^. mainData)

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
      m <- parse parseHeader $ flattenKeywords h
      Map.size m @?= 1


    describe "sample header file" $ do
      bs <- liftIO $ BS.readFile "./fits_files/nso_dkist_headers.txt"
      let ts = filter (not . ignore) $ T.lines $ TE.decodeUtf8 bs

      it "should parse all keywords individually" $ do
        forM_ ( zip [1..] ts ) $ \(n, t) -> do
          _ <- parse parseRecordLine $ flattenKeywords [TE.encodeUtf8 t]
          pure ()

      it "should parse NAxes correctly" $ do
        (sz, _) <- parse parseBinTableKeywords $ mconcat $ C8.lines bs
        (sz ^. axes) @?= [32, 998]

  where
    ignore t = T.isPrefixOf "CONTINUE" t || T.isPrefixOf "END" t



sampleNSO :: Test ()
sampleNSO = do
  describe "NSO Sample FITS Parse" $ do
    bs <- liftIO $ BS.readFile "./fits_files/nso_dkist.fits"
    it "should parse empty primary header" $ do
      h0 <- eitherFail $ readPrimaryHDU bs
      -- first header doesn't have any data
      BS.length (h0 ^. mainData) @?= 0

    it "should parse both HDUs" $ do
      hdus <- eitherFail $ readHDUs bs
      length hdus @?= 2
      [_, h2] <- pure hdus
      Fits.lookup "INSTRUME" (h2 ^. header) @?= Just (String "VISP")
      Fits.lookup "NAXIS" (h2 ^. header) @?= Just (Integer 2)

      let sizeOnDisk = 161280
          countedHeaderBlocks = 11 -- this was manually counted... until end of all headers
          payloadLength = BS.length (h2 ^. mainData)
          headerLength = countedHeaderBlocks * hduBlockSize
          heapLength = pCount (h2 ^. extension)

      -- Payload size is as expected
      payloadLength @?= 32 * 998 * fromIntegral (bitPixToByteSize EightBitInt)
      pCount (h2 ^. extension) @?= 95968
      assertBool "The end of the heap has some null data" $ do
        C8.all (/='\0') $ C8.take 100 $ C8.drop (headerLength + payloadLength + heapLength - 100) bs
      assertBool "The remainder of the file contains real data" $ do
        C8.all (=='\0') $ C8.drop (headerLength + payloadLength + heapLength) bs
      where
        pCount :: Extension -> Int
        pCount (BinTable p h) = p
        pCount _ = 0

-- Test monad with describe/it
newtype Test a = Test {runTest :: WriterT [TestTree] IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadWriter [TestTree])

runTests :: TestName -> Test () -> IO TestTree
runTests n (Test t) = do
    tests <- execWriterT t :: IO [TestTree]
    return $ testGroup n tests


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



