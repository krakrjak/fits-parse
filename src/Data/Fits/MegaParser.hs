{-|
Module      : Data.Fits.MegaParser
Description : MegaParsec based parser for an HDU.
Copyright   : (c) Zac Slade, 2018
License     : BSD2
Maintainer  : krakrjak@gmail.com
Stability   : experimental

Parsing rules for an HDU in a FITS file.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Fits.MegaParser where

-- qualified imports
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS ( c2w )
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as M

-- explicit imports
import Numeric.Natural ( Natural )
import Data.ByteString ( ByteString )
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Void ( Void )
import Data.Default ( def )
import Text.Ascii ( isAscii )

-- full module imports
import Text.Megaparsec ( Parsec, ParseError, (<|>))

-- local imports
import Data.Fits

type Parser = Parsec Void ByteString
type ParseErr = ParseError Word8 Void

data HeaderValues
  = FITSStrStrKwd StringValue StringValue
  | FITSStrNumKwd StringValue NumberValue
  | END

data DataUnitValues
  = FITSUInt8 Word8
  | FITSInt16 Word16
  | FITSInt32 Word32
  | FITSInt64 Word64
  | FITSFloat32 Float
  | FITSFloat64 Double

headerBlockParse :: Parser HeaderData
headerBlockParse = do
    simple <- parseSimple
    bitpix <- parseBitPix
    parseEnd
    return defHeader { simpleFormat = simple
                     , bitPixFormat = bitpix }
  where
    defHeader = def :: HeaderData

parseSimple :: Parser SimpleFormat
parseSimple = M.string' "simple" >> parseEquals
    >> ((conformParse >> consumeDead >> return Conformant)
    <|> (nonConformParse >> consumeDead >> return NonConformant))
  where
    conformParse = M.string' "t"
    nonConformParse = M.anyChar

parseEquals :: Parser ()
parseEquals = M.space >> M.char (BS.c2w '=') >> M.space

parseBitPix :: Parser BitPixFormat
parseBitPix = M.string' "bitpix" >> parseEquals
    >> ((M.string' "8" >> consumeDead >> return EightBitInt)
    <|> (M.string' "16" >> consumeDead >> return SixteenBitInt)
    <|> (M.string' "32" >> consumeDead >> return ThirtyTwoBitInt)
    <|> (M.string' "64" >> consumeDead >> return SixtyFourBitInt)
    <|> (M.string' "-32" >> consumeDead >> return ThirtyTwoBitFloat)
    <|> (M.string' "-64" >> consumeDead >> return SixtyFourBitFloat))

skipEmpty :: Parser ()
skipEmpty = (M.many $ M.satisfy ((0::Word8) ==)) >> return ()

consumeDead :: Parser ()
consumeDead = M.space >> skipEmpty

parseEnd :: Parser ()
parseEnd = M.string' "end" >> consumeDead

countHeaderDataUnits :: ByteString -> IO Natural
countHeaderDataUnits bs = getAllHDUs bs >>= return . fromIntegral . length

getAllHDUs :: ByteString -> IO [HeaderDataUnit]
getAllHDUs bs = do
    (hdu, rest) <- getOneHDU bs
    case BS.length rest < hduBlockSize of
      True  -> return [hdu]
      False -> fmap ([hdu] ++) $ getAllHDUs rest

getOneHDU :: ByteString -> IO (HeaderDataUnit, ByteString)
getOneHDU bs =
    case isAscii header of
      False -> error "Header data is not ASCII. Please Check your input file and try again"
      True  -> do
        case M.runParser headerBlockParse "FITS" header of
          Right mainHeader -> do
            let (dataUnit, remainder) = BS.splitAt (fromIntegral $ dataSize mainHeader) rest
            return (HeaderDataUnit mainHeader dataUnit, remainder)
          Left e -> let err = M.parseErrorPretty e in error err
  where
    (header, rest) = BS.splitAt hduBlockSize bs

dataSize :: HeaderData -> Natural
dataSize h = paddedsize h
  where
    wordsize h = fromIntegral . bitPixToWordSize $ bitPixFormat h
    axesCount h = fromIntegral . length $ axes h
    datasize h = wordsize h * axesCount h
    padding h = if axesCount h == 0 then 0 else fromIntegral hduBlockSize - (datasize h `mod` (fromIntegral hduBlockSize))
    paddedsize h = fromIntegral (datasize h + padding h)
