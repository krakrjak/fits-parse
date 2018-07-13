{-|
Module      : MegaParser
Description : MegaParsec based parser for an HDU.
Copyright   : (c) Zac Slade, 2018
License     : BSD2
Maintainer  : krakrjak@gmail.com
Stability   : experimental

Parsing rules for an HDU in a FITS file.
-}

module Data.Fits.MegaParser where

-- qualified imports
import qualified Data.ByteString as BS
import qualified Text.Megaparsec.Byte.Lexer as L

-- explicit imports
import Data.ByteString ( ByteString )
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Void ( Void )
import Numeric.Natural ( Natural )
import Text.Ascii ( isAscii )

-- full module imports
import Text.Megaparsec

-- local imports
import Data.Fits

type Parser = Parsec Void ByteString
type ParseErr = ParseError Word8 Void

data HeaderValues
  = FITSStrKwd StringValue StringValue
  | FITSNumKwd NumberValue
  | END

data DataUnitValues
  = FITSUInt8 Word8
  | FITSInt16 Word16
  | FITSInt32 Word32
  | FITSInt64 Word64
  | FITSFloat32 Float
  | FITSFloat64 Double

countHeaderDataUnits :: ByteString -> IO Natural
countHeaderDataUnits bs = undefined -- parseHeader bs <|>

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
        mainHeader <- runParser headerBlockParse header
        let (dataUnit, remainder) = BS.splitAt (paddedsize mainHeader) rest
        return (HeaderDataUnit mainHeader dataUnit, remainder)
  where
    (header, rest) = BS.splitAt hduBlockSize bs
    wordsize h = bitPixToWordSize $ bitPixFormat h
    axesCount h = length $ axes h
    datasize h = wordsize h * axesCount h
    padding h = hduBlockSize - (datasize h `mod` hduBlockSize)
    paddedsize h = datasize h + padding h
