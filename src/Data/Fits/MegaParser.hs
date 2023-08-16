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
import Data.Map ( Map )
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
import qualified Data.Text.Encoding as C8
import qualified Data.Binary as C8

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
parseAllKeywords :: Parser (Map Keyword Value)
parseAllKeywords = do
    pairs <- M.manyTill parseRecordLine (M.string' "end")
    M.space -- consume space padding all the way to the end of the next 2880 bytes header block
    return $ Map.fromList $ catMaybes pairs

parseRecordLine :: Parser (Maybe (Keyword, Value))
parseRecordLine = do
    M.try $ Just <$> parseKeywordRecord
    <|> Nothing <$ parseLineComment
    <|> Nothing <$ M.string' (BS.replicate hduRecordLength (toWord ' '))

-- | Combinator to allow for parsing a record with inline comments
withComments :: Parser a -> Parser a
withComments parseKV = do
    start <- parsePos
    kv <- parseKV
    M.space
    M.optional $ parseInlineComment start
    M.space
    return kv

parseKeywordRecord :: Parser (Keyword, Value)
parseKeywordRecord = withComments parseKeywordValue

parseKeywordValue :: Parser (Keyword, Value)
parseKeywordValue = do
    key <- parseKeyword
    parseEquals
    val <- parseValue
    -- traceM $ show key <> " = " <> show val
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
    M.try (Float <$> parseFloat)
    <|> M.try (Integer <$> parseInt)
    <|> (Logic <$> parseLogic)
    <|> (String <$> parseStringContinue)

parseInt :: Num a => Parser a
parseInt = MBL.signed M.space MBL.decimal

parseFloat :: Parser Float
parseFloat = MBL.signed M.space MBL.float

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



requireKeyword :: Keyword -> Header -> Parser Value
requireKeyword k kvs = do
    case Fits.lookup k kvs of
      Nothing -> fail $ "Missing: " <> show k
      Just v -> return v
            



requireNaxis :: Header -> Parser Int
requireNaxis kvs = do
    v <- requireKeyword "NAXIS" kvs
    case v of
      Integer n -> return n
      _ -> fail "Invalid NAXIS header"



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

parsePos :: Parser Int
parsePos = MP.unPos . MP.sourceColumn <$> M.getSourcePos


parseSimple :: Parser SimpleFormat
parseSimple = withComments $ do
    M.string' "SIMPLE"
    parseEquals
    parseLogic
    return Conformant

parseExtension :: Parser Text
parseExtension = withComments $ do
    M.string' "XTENSION"
    parseEquals
    parseStringValue

parseBitPix :: Parser BitPixFormat
parseBitPix = withComments $ do
    M.string' "BITPIX"
    parseEquals
    v <- parseValue
    toBitpix v
    where
      toBitpix (Integer 8) = return EightBitInt
      toBitpix (Integer 16) = return SixteenBitInt
      toBitpix (Integer 32) = return ThirtyTwoBitInt
      toBitpix (Integer 64) = return SixtyFourBitInt
      toBitpix (Integer (-32)) = return ThirtyTwoBitFloat
      toBitpix (Integer (-64)) = return SixtyFourBitFloat
      toBitpix _ = fail "Invalid BITPIX header"

parseNaxes :: Parser NAxes
parseNaxes = do
    n <- parse
    ax <- mapM parseN [1..n]
    return $ NAxes ax

    where
      parse :: Parser Int
      parse = withComments $ do
        M.string' "NAXIS"
        parseEquals
        parseInt

      parseN :: Int -> Parser Natural
      parseN n = withComments $ do
        M.string' "NAXIS"
        M.string' $ BS.pack $ map toWord (show n)
        parseEquals
        parseInt


-- | We don't parse simple here, because it isn't required on all HDUs
parseSizeKeywords :: Parser SizeKeywords
parseSizeKeywords = do
    bp <- parseBitPix
    ax <- parseNaxes
    return $ SizeKeywords { bitpix = bp, naxes = ax }


parseHeader :: Parser Header
parseHeader = do
    -- this consumes all the way up to the end of the header
    -- TODO: handle simple vs image xtension, etc. Each is required differently
    void parseExtension <|> void parseSimple
    sz <- M.lookAhead parseSizeKeywords
    kvs <- parseAllKeywords
    return $ Header { size = sz, keywords = kvs }
  

parseHDU :: Parser HeaderDataUnit
parseHDU = do
    h <- parseHeader

    -- now grab the data array
    let len = dataSize h.size
    da <- M.takeP (Just ("Data Array of " <> show len <> " Bytes")) (fromIntegral len)
    return $ HeaderDataUnit h da

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


