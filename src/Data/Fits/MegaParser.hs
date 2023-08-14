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

module Data.Fits.MegaParser where

import Debug.Trace

-- qualified imports
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS ( c2w )
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Stream as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Pos as MP
import qualified Text.Megaparsec.Byte as MB
import qualified Text.Megaparsec.Char.Lexer as MCL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map as Map

-- explicit imports
import Control.Applicative ( (<$>) )
import Control.Monad ( void, foldM )
import Numeric.Natural ( Natural )
import Data.ByteString ( ByteString )
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

-- local imports
import Data.Fits

type Parser = Parsec Void ByteString
type ParseErr = ParseErrorBundle Text Void

data DataUnitValues
  = FITSUInt8 Word8
  | FITSInt16 Word16
  | FITSInt32 Word32
  | FITSInt64 Word64
  | FITSFloat32 Float
  | FITSFloat64 Double 




-- | Consumes ALL header blocks until end, then all remaining space
parseHeader :: Parser Header
parseHeader = do
    pairs <- M.manyTill parseKeywordRecord (M.string' "end")
    M.space -- consume space padding all the way to the end of the next 2880 bytes header block
    return $ Map.fromList pairs

parseKeywordValue :: Parser (Keyword, Value)
parseKeywordValue = do
    key <- parseKeyword
    parseEquals
    val <- parseValue
    -- M.optional M.newline
    -- M.optional M.eof
    return (key, val)

parseKeywordRecord :: Parser (Keyword, Value)
parseKeywordRecord = do
    start <- parsePos
    kv <- parseKeywordValue
    M.space
    M.optional $ parseComment start
    M.space
    return kv

parsePos :: Parser Int
parsePos = MP.unPos . MP.sourceColumn <$> M.getSourcePos

parseComment :: Int -> Parser Comment
parseComment start = do
    M.char '/'
    M.space
    com <- parsePos
    let end = start + 80
    let rem = end - com
    c <- M.count rem M.anySingle
    return $ Comment (T.pack c)



parseKeyword :: Parser Keyword
parseKeyword = Keyword <$> parseText

parseValue :: Parser Value
parseValue = 
    -- try is required here because Megaparsec doesn't automatically backtrack if the parser consumes anything
    M.try (Float <$> MCL.signed M.space MCL.float)
    <|> M.try (Integer <$> MCL.signed M.space MCL.decimal)
    <|> (String <$> parseStringValue)
    <|> (Logic <$> parseLogic)

parseLogic :: Parser LogicalConstant
parseLogic = do
    M.string' "T"
    pure T

parseText :: Parser Text
parseText = do
    T.pack <$> M.many M.alphaNumChar


-- | We don't parse simple here, because it isn't required on all HDUs
parseSizeKeywords :: Header -> Parser SizeKeywords
parseSizeKeywords kvs = do
    bp <- parseBitPix kvs
    ax <- parseNaxes kvs
    return $ SizeKeywords { bitpix = bp, naxes = ax }

requireKeyword :: Keyword -> Header -> Parser Value
requireKeyword k kvs = do
    case Map.lookup k kvs of
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

parseAxisCount :: Parser Natural
parseAxisCount = M.string' "naxis" >> parseEquals >> parseNatural



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
skipEmpty = void (M.many $ M.satisfy ('\0' ==))

consumeDead :: Parser ()
consumeDead = M.space >> skipEmpty

parseEnd :: Parser ()
parseEnd = M.string' "end" >> M.space <* M.eof

parseEquals :: Parser ()
parseEquals = M.space >> M.char '=' >> M.space

parseNatural :: Parser Natural
parseNatural = do
    v <- MCL.decimal
    consumeDead
    return $ fromIntegral v

parseInteger :: Parser Int
parseInteger = do
    v <- MCL.decimal
    consumeDead
    return v

parseStringValue :: Parser Text
parseStringValue = do
    -- The rules are weird, NULL means a NULL string, '' is an empty
    -- string, a ' followed by a bunch of spaces and a close ' is
    -- considered an empty string, and trailing whitespace is ignored
    -- within the quotes, but not leading spaces.
    ls <- M.between (M.char quote) (M.char quote) $ M.many $ M.anySingleBut quote
    consumeDead
    return (T.pack ls)
    where quote = '\''


parseHDU :: Parser HeaderDataUnit
parseHDU = do
    -- this consumes all the way up to the end of the header
    h <- parseHeader
    sz <- parseSizeKeywords h

    -- now grab the data array
    let len = dataSize sz
    da <- M.takeP (Just ("Data Array of " <> show len <> " Bytes")) len
    return $ HeaderDataUnit h size da



-- countHeaderDataUnits :: ByteString -> IO Natural
-- countHeaderDataUnits bs = fromIntegral . length <$> getAllHDUs bs

-- -- TODO: make the recursive case work. Currently limited to one HDU.
-- -- The current issue is that when the parser fails on an HDU parse, it
-- -- blows them all up instead of accepting the valid parsings.
-- getAllHDUs :: ByteString -> IO [HeaderDataUnit]
-- getAllHDUs bs = do
--     (hdu, rest) <- getOneHDU bs
--     return [hdu]

--    if BS.length rest < hduBlockSize then return [hdu] else return [hdu]
getOneHDU :: ByteString -> IO (HeaderDataUnit, ByteString)
getOneHDU bs =
    if isAscii header
      then
        case M.runParser parseHeaderSize "FITS" (TE.decodeUtf8 header) of
          Right (mainHeader, size) -> do
            let (dataUnit, remainder) = BS.splitAt (fromIntegral $ dataSize size) rest
            return (HeaderDataUnit mainHeader size dataUnit, remainder)
          Left e -> let err = M.errorBundlePretty e in error err
      else error "Header data is not ASCII. Please Check your input file and try again"
  where
    -- TODO: this won't work. headers can span multiple blocks
    -- can we consume the ByteString directly?
    (header, rest) = BS.splitAt hduBlockSize bs

dataSize :: SizeKeywords -> Natural
dataSize h = wordSize * wordCount
  where
    -- paddedsize

    wordCount = fromIntegral $ product $ axes $ naxes h
    wordSize = fromIntegral . bitPixToWordSize $ bitpix h

--   datasize = wordsize * wordCount
--   padding = if axesCount == 0 then 0 else fromIntegral hduBlockSize - datasize `mod` fromIntegral hduBlockSize
--   paddedsize = fromIntegral (datasize + padding)
