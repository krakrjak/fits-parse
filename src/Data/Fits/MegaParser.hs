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

-- qualified imports
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS ( c2w )
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Stream as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as MCL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map as Map

-- explicit imports
import Control.Applicative ( (<$>) )
import Control.Monad ( void )
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

type Parser = Parsec Void Text
type ParseErr = ParseErrorBundle Text Void

data DataUnitValues
  = FITSUInt8 Word8
  | FITSInt16 Word16
  | FITSInt32 Word32
  | FITSInt64 Word64
  | FITSFloat32 Float
  | FITSFloat64 Double 



parseRawHeader :: Parser RawHeaderData
parseRawHeader = do
    pairs <- M.manyTill ( parseKeywordValue <* M.newline ) (M.string "END")
    consumeDead
    pure $ Map.fromList pairs

parseKeywordValue :: Parser (Keyword, Value)
parseKeywordValue = do
    key <- parseKeyword
    parseEquals
    val <- parseValue
    pure (key, val)

parseKeyword :: Parser Keyword
parseKeyword = Keyword <$> parseText

parseValue :: Parser Value
parseValue = 
    (M.try $ Float <$> MCL.signed M.space MCL.float) <|>
    (M.try $ Integer <$> MCL.signed M.space MCL.decimal) <|>
    (String <$> parseStringValue)

parseText :: Parser Text
parseText = do
    T.pack <$> M.many M.alphaNumChar


headerBlockParse :: Parser HeaderData
headerBlockParse = do
    -- TODO: Parse other sections of the neader for god's sake
    --  HISTORY and COMMENT along with CONTINUE handling is missing
    (simple, bitpix, axes, pEnd) <-
        runPermutation $
            (,,,)
                <$> toPermutation (parseSimple <?> "simple")
                <*> toPermutation (parseBitPix <?> "bitpix")
                <*> toPermutation ((parseAxisCount >>= parseNaxes) <?> "axis parsing")
                -- <*> toPermutationWithDefault 0 (parseBzero <?> "bzero")
                -- <*> toPermutationWithDefault 0 (parseBscale <?> "bscale")
                -- <*> toPermutationWithDefault (StringValue NullString Nothing) (parseReference <?> "reference")
                -- <*> toPermutationWithDefault (StringValue NullString Nothing) (parseObserver <?> "observer")
                -- <*> toPermutationWithDefault (StringValue NullString Nothing) (parseInstrument <?> "instrument")
                -- <*> toPermutationWithDefault (StringValue NullString Nothing) (parseTelescope <?> "telescope")
                -- <*> toPermutationWithDefault (StringValue NullString Nothing) (parseObject <?> "object")
                -- <*> toPermutationWithDefault (StringValue NullString Nothing) (parseCreator <?> "creator")
                -- <*> toPermutationWithDefault (StringValue NullString Nothing) (parseDate <?> "date")
                <*> toPermutation (parseEnd <?> "end")
    return
        HeaderData
            { simple = simple
            , bitpix = bitpix
            , naxes = axes
            }

parseSimple :: Parser SimpleFormat
parseSimple =
    M.string' "simple"
        >> parseEquals
        >> (conformParse >> consumeDead >> return Conformant)
        -- <|> (nonConformParse >> consumeDead >> return NonConformant)
  where
    conformParse = M.char' 't'
    -- nonConformParse = M.anySingle

parseEquals :: Parser ()
parseEquals = M.space >> M.char '=' >> M.space

parseBitPix :: Parser BitPixFormat
parseBitPix  = M.string' "bitpix" >> parseEquals
    >> ((M.chunk "8" >> consumeDead >> return EightBitInt)
    <|> (M.chunk "16" >> consumeDead >> return SixteenBitInt)
    <|> (M.chunk "32" >> consumeDead >> return ThirtyTwoBitInt)
    <|> (M.chunk "64" >> consumeDead >> return SixtyFourBitInt)
    <|> (M.chunk "-32" >> consumeDead >> return ThirtyTwoBitFloat)
    <|> (M.chunk "-64" >> consumeDead >> return SixtyFourBitFloat))

parseAxisCount :: Parser Natural
parseAxisCount = M.string' "naxis" >> parseEquals >> parseNatural

parseNaxes :: Natural -> Parser NAxes
parseNaxes n | n == 0 = return (NAxes [])
parseNaxes n = do
    axisNum <- M.string' "naxis" >> parseNatural
    elemCount <- parseEquals >> parseNatural
    rest <- axes <$> parseNaxes (n - 1)
    return $ NAxes $ elemCount : rest

parseBzero :: Parser Int
parseBzero = M.string' "bzero" >> parseEquals >> parseInteger

parseBscale :: Parser Int
parseBscale = M.string' "bscale" >> parseEquals >> parseInteger

parseReference :: Parser Text
parseReference = M.string' "referenc" >> parseEquals >> parseStringValue

parseObserver :: Parser Text
parseObserver = M.string' "observer" >> parseEquals >> parseStringValue

parseInstrument :: Parser Text
parseInstrument = M.string' "instrume" >> parseEquals >> parseStringValue

parseTelescope :: Parser Text
parseTelescope = M.string' "telescop" >> parseEquals >> parseStringValue

parseObject :: Parser Text
parseObject = M.string' "object" >> parseEquals >> parseStringValue

parseCreator :: Parser Text
parseCreator = M.string' "creator" >> parseEquals >> parseStringValue

parseDate :: Parser Text
parseDate = M.string' "date" >> parseEquals >> parseStringValue

skipEmpty :: Parser ()
skipEmpty = void (M.many $ M.satisfy ('\0' ==))

consumeDead :: Parser ()
consumeDead = M.space >> skipEmpty

parseEnd :: Parser ()
parseEnd = M.string' "end" >> M.space <* M.eof

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

countHeaderDataUnits :: ByteString -> IO Natural
countHeaderDataUnits bs = fromIntegral . length <$> getAllHDUs bs

-- TODO: make the recursive case work. Currently limited to one HDU.
-- The current issue is that when the parser fails on an HDU parse, it
-- blows them all up instead of accepting the valid parsings.
getAllHDUs :: ByteString -> IO [HeaderDataUnit]
getAllHDUs bs = do
    (hdu, rest) <- getOneHDU bs
    return [hdu]

--    if BS.length rest < hduBlockSize then return [hdu] else return [hdu]
getOneHDU :: ByteString -> IO (HeaderDataUnit, ByteString)
getOneHDU bs =
    if isAscii header
      then
        case M.runParser headerBlockParse "FITS" (TE.decodeUtf8 header) of
          Right mainHeader -> do
            let (dataUnit, remainder) = BS.splitAt (fromIntegral $ dataSize mainHeader) rest
            return (HeaderDataUnit mainHeader dataUnit, remainder)
          Left e -> let err = M.errorBundlePretty e in error err
      else error "Header data is not ASCII. Please Check your input file and try again"
  where
    (header, rest) = BS.splitAt hduBlockSize bs

dataSize :: HeaderData -> Natural
dataSize h = wordSize * wordCount
  where
    -- paddedsize

    wordCount = fromIntegral $ product $ axes $ naxes h
    wordSize = fromIntegral . bitPixToWordSize $ bitpix h

--   datasize = wordsize * wordCount
--   padding = if axesCount == 0 then 0 else fromIntegral hduBlockSize - datasize `mod` fromIntegral hduBlockSize
--   paddedsize = fromIntegral (datasize + padding)
