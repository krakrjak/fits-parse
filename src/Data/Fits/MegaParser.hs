{-|
Module      : Data.Fits.MegaParser
Description : MegaParsec based parser for an HDU.
Copyright   : (c) Zac Slade, 2018
License     : BSD2
Maintainer  : krakrjak@gmail.com
Stability   : experimental

Parsing rules for an HDU in a FITS file.
-}

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Data.Fits.MegaParser where

import Debug.Trace

-- qualified imports
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS ( c2w )
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Stream as M
import qualified Text.Megaparsec.Pos as MP
import qualified Text.Megaparsec.Byte as M
import qualified Text.Megaparsec.Byte.Lexer as MBL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map as Map

-- explicit imports
import Control.Applicative ( (<$>) )
import Control.Monad ( void, foldM )
import Control.Exception ( Exception(displayException) )
import Numeric.Natural ( Natural )
import Data.Bifunctor ( first )
import Data.ByteString ( ByteString )
import Data.Char ( ord )
import Data.Text ( Text )
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Void ( Void )
import Data.Default ( def )
import Text.Ascii ( isAscii )
import Text.Megaparsec ( Parsec, ParseErrorBundle, (<|>), (<?>))

-- full module imports
---- parser-combinators
import Control.Applicative.Permutations
---- base
import Data.Proxy
import Data.Maybe ( catMaybes, fromMaybe )

-- local imports
import Data.Fits
import qualified Data.Fits as Fits

type Parser = Parsec Void ByteString
type ParseErr = ParseErrorBundle ByteString Void

data DataUnitValues
  = FITSUInt8 Word8
  | FITSInt16 Word16
  | FITSInt32 Word32
  | FITSInt64 Word64
  | FITSFloat32 Float
  | FITSFloat64 Double 


toWord :: Char -> Word8
toWord = fromIntegral . ord

toText :: [Word8] -> Text
toText = TE.decodeUtf8 . BS.pack



-- | Consumes ALL header blocks until end, then all remaining space
parseHeader :: Parser Header
parseHeader = do
    pairs <- M.manyTill parseRecordLine (M.string' "end")
    M.space -- consume space padding all the way to the end of the next 2880 bytes header block
    return $ Header $ Map.fromList $ catMaybes pairs

parseRecordLine :: Parser (Maybe (Keyword, Value))
parseRecordLine = do
    M.try $ Just <$> parseKeywordRecord
    <|> Nothing <$ parseLineComment
    <|> Nothing <$ M.string' (BS.replicate hduRecordLength (toWord ' '))

parseKeywordRecord :: Parser (Keyword, Value)
parseKeywordRecord = do
    start <- parsePos
    kv <- parseKeywordValue
    M.space
    M.optional $ parseInlineComment start
    M.space
    return kv

parseKeywordValue :: Parser (Keyword, Value)
parseKeywordValue = do
    key <- parseKeyword
    parseEquals
    val <- parseValue
    return (key, val)

parseInlineComment :: Int -> Parser Comment
parseInlineComment start = do
    M.char $ toWord '/'
    M.space
    com <- parsePos
    let end = start + hduRecordLength
    let rem = end - com
    c <- M.count rem M.anySingle
    return $ Comment (toText c)

parseLineComment :: Parser Comment
parseLineComment = do
    let keyword = "COMMENT " :: ByteString
    M.string' keyword
    c <- M.count (hduRecordLength - BS.length keyword) M.anySingle
    return $ Comment (toText c)

-- | Anything but a space or equals
parseKeyword :: Parser Keyword
parseKeyword = Keyword . toText <$> M.some (M.noneOf $ fmap toWord [' ', '='])

parseValue :: Parser Value
parseValue =
    -- try is required here because Megaparsec doesn't automatically backtrack if the parser consumes anything
    M.try (Float <$> MBL.signed M.space MBL.float)
    <|> M.try (Integer <$> MBL.signed M.space MBL.decimal)
    <|> (Logic <$> parseLogic)
    <|> (String <$> parseStringContinue)
    where
      parseLogic :: Parser LogicalConstant
      parseLogic = do
          M.string' "T"
          return T

parseStringContinue :: Parser Text
parseStringContinue = do
    t <- parseStringValue

    mc <- M.optional $ do
      M.string' "CONTINUE"
      M.space
      parseStringContinue

    case mc of
      Nothing -> return t
      Just tc -> return $ T.dropWhileEnd (=='&') t <> tc


-- | We don't parse simple here, because it isn't required on all HDUs
parseSizeKeywords :: Header -> Parser SizeKeywords
parseSizeKeywords kvs = do
    bp <- parseBitPix kvs
    ax <- parseNaxes kvs
    return $ SizeKeywords { bitpix = bp, naxes = ax }

requireKeyword :: Keyword -> Header -> Parser Value
requireKeyword k kvs = do
    case Fits.lookup k kvs of
      Nothing -> fail $ "Missing: " <> show k
      Just v -> return v
            

parseSimple :: Header -> Parser SimpleFormat
parseSimple kvs = do
    v <- requireKeyword "SIMPLE" kvs
    case v of
      Logic T -> return Conformant
      _ -> fail "Invalid Keyword: SIMPLE"


parseBitPix :: Header -> Parser BitPixFormat
parseBitPix kvs = do
    bpn <- requireKeyword "BITPIX" kvs
    toBitpix bpn
    where
      toBitpix (Integer 8) = return EightBitInt
      toBitpix (Integer 16) = return SixteenBitInt
      toBitpix (Integer 32) = return ThirtyTwoBitInt
      toBitpix (Integer 64) = return SixtyFourBitInt
      toBitpix (Integer (-32)) = return ThirtyTwoBitFloat
      toBitpix (Integer (-64)) = return SixtyFourBitFloat
      toBitpix _ = fail "Invalid BITPIX header"


requireNaxis :: Header -> Parser Int
requireNaxis kvs = do
    v <- requireKeyword "NAXIS" kvs
    case v of
      Integer n -> return n
      _ -> fail "Invalid NAXIS header"


parseNaxes :: Header -> Parser NAxes
parseNaxes kvs = do
    n <- requireNaxis kvs
    as <- mapM naxisn (keywords n)
    return $ NAxes as

    where
      keywords :: Int -> [Keyword]
      keywords n = fmap (Keyword . ("NAXIS"<>) . T.pack . show) [1..n]

      naxisn :: Keyword -> Parser Natural
      naxisn k = do
        n <- requireKeyword k kvs
        case n of
          (Integer n) -> return (fromIntegral n)
          _ -> fail $ "Invalid: " <> show k

-- TODO: replace these with known headers

-- parseBzero :: Parser Int
-- parseBzero = M.string' "bzero" >> parseEquals >> parseInteger
--
-- parseBscale :: Parser Int
-- parseBscale = M.string' "bscale" >> parseEquals >> parseInteger
--
-- parseReference :: Parser Text
-- parseReference = M.string' "referenc" >> parseEquals >> parseStringValue
--
-- parseObserver :: Parser Text
-- parseObserver = M.string' "observer" >> parseEquals >> parseStringValue
--
-- parseInstrument :: Parser Text
-- parseInstrument = M.string' "instrume" >> parseEquals >> parseStringValue
--
-- parseTelescope :: Parser Text
-- parseTelescope = M.string' "telescop" >> parseEquals >> parseStringValue
--
-- parseObject :: Parser Text
-- parseObject = M.string' "object" >> parseEquals >> parseStringValue
--
-- parseCreator :: Parser Text
-- parseCreator = M.string' "creator" >> parseEquals >> parseStringValue
--
-- parseDate :: Parser Text
-- parseDate = M.string' "date" >> parseEquals >> parseStringValue

skipEmpty :: Parser ()
skipEmpty = void (M.many $ M.satisfy (toWord '\0' ==))

consumeDead :: Parser ()
consumeDead = M.space >> skipEmpty

parseEnd :: Parser ()
parseEnd = M.string' "end" >> M.space <* M.eof

parseEquals :: Parser ()
parseEquals = M.space >> M.char (toWord '=') >> M.space

parseStringValue :: Parser Text
parseStringValue = do
    -- The rules are weird, NULL means a NULL string, '' is an empty
    -- string, a ' followed by a bunch of spaces and a close ' is
    -- considered an empty string, and trailing whitespace is ignored
    -- within the quotes, but not leading spaces.
    ls <- M.between (M.char quote) (M.char quote) $ M.many $ M.anySingleBut quote
    consumeDead
    return (T.stripEnd $ toText ls)
    where quote = toWord '\''

parsePos :: Parser Int
parsePos = MP.unPos . MP.sourceColumn <$> M.getSourcePos


parseHDU :: Parser HeaderDataUnit
parseHDU = do
    -- this consumes all the way up to the end of the header
    h <- parseHeader
    sz <- parseSizeKeywords h

    -- now grab the data array
    let len = dataSize sz
    da <- M.takeP (Just ("Data Array of " <> show len <> " Bytes")) (fromIntegral len)
    return $ HeaderDataUnit h sz da

parseHDUs :: Parser [HeaderDataUnit]
parseHDUs = do
    M.many parseHDU

getAllHDUs :: ByteString -> Either FitsError [HeaderDataUnit]
getAllHDUs bs = do
    first ParseError $ M.runParser parseHDUs "FITS" bs

getOneHDU :: ByteString -> Either FitsError HeaderDataUnit
getOneHDU bs = do
    first ParseError $ M.runParser parseHDU "FITS" bs

dataSize :: SizeKeywords -> Natural
dataSize h = size h.bitpix * count h.naxes
  where
    count (NAxes []) = 0
    count (NAxes ax) = fromIntegral $ product ax
    size = fromIntegral . bitPixToByteSize



newtype FitsError
    = ParseError ParseErr
    deriving (Eq)

instance Show FitsError where
    show (ParseError e) = displayException e



-- 1st. Which dataset can we do. Put it where they can use it
-- 2nd. When they have made a L2 data product. How do we get that back in such a way that we can put it on the protal
-- 3rd. Make the middle part automatic.
