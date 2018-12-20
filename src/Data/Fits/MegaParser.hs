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
import qualified Text.Megaparsec.Stream as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as MCL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- explicit imports
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

headerBlockParse :: Parser HeaderData
headerBlockParse = do
    -- TODO: Parse other sections of the neader for god's sake
    (simple, bitpix, axesDesc, bZero, bScale, pCreator, pDate, pEnd) <- runPermutation $
        (,,,,,,,) <$> toPermutation (parseSimple <?> "simple")
               <*> toPermutation (parseBitPix <?> "bitpix")
               <*> toPermutation ((parseAxisCount >>= parseNaxes) <?> "axis parsing")
               <*> toPermutationWithDefault 0 (parseBzero <?> "bzero")
               <*> toPermutationWithDefault 0 (parseBscale <?> "bscale")
               <*> toPermutationWithDefault (StringValue NullString Nothing) (parseCreator <?> "creator")
               <*> toPermutationWithDefault (StringValue NullString Nothing) (parseDate <?> "date")
               <*> toPermutation (parseEnd <?> "end")
    return defHeader { simpleFormat = simple
                     , bitPixFormat = bitpix
                     , axes = axesDesc
                     , observationDate = pDate
                     , authorIdentifier = pCreator }
  where
    defHeader = def :: HeaderData

parseSimple :: Parser SimpleFormat
parseSimple = M.string' "simple" >> parseEquals
    >> ((conformParse >> consumeDead >> return Conformant)
    <|> (nonConformParse >> consumeDead >> return NonConformant))
  where
    conformParse = M.char' 't'
    nonConformParse = M.anySingle

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

parseNaxes :: Natural -> Parser [Axis]
parseNaxes n | n == 0 = return []
parseNaxes n          = do
    axisNum <- M.string' "naxis" >> parseNatural
    elemCount <- parseEquals >> parseNatural
    fmap ([buildAxis axisNum elemCount] ++) $ parseNaxes (n - 1)
  where
      defAxis = def :: Axis
      buildAxis an ec = defAxis { axisNumber = fromIntegral an
                                , axisElementCount = fromIntegral ec }

parseBzero :: Parser Int
parseBzero = M.string' "bzero" >> parseEquals >> parseInteger

parseBscale :: Parser Int
parseBscale = M.string' "bscale" >> parseEquals >> parseInteger

parseCreator :: Parser StringValue
parseCreator = M.string' "creator" >> parseEquals >> parseStringValue

parseDate :: Parser StringValue
parseDate = M.string' "date" >> parseEquals >> parseStringValue

skipEmpty :: Parser ()
skipEmpty = (M.many $ M.satisfy (('\0') ==)) >> return ()

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

parseStringValue :: Parser StringValue
parseStringValue = do 
    -- The rules are weird, NULL means a NULL string, '' is an empty
    -- string, a ' followed by a bunch of spaces and a close ' is
    -- considered an empty string, and trailing whitespace is ignored
    -- within the quotes, but not leading spaces.
    ls <- M.between (M.char '\'') (M.char '\'') $ M.many $ M.anySingleBut '\''
    consumeDead
    let v = M.tokensToChunk (Proxy :: Proxy Text) ls
    if T.length v < 1
      then return (StringValue EmptyString Nothing)
      else return (StringValue DataString (Just v))

countHeaderDataUnits :: ByteString -> IO Natural
countHeaderDataUnits bs = getAllHDUs bs >>= return . fromIntegral . length

-- TODO: make the recursive case work. Currently limited to one HDU.
-- The current issue is that when the parser fails on an HDU parse, it
-- blows them all up instead of accepting the valid parsings.
getAllHDUs :: ByteString -> IO [HeaderDataUnit]
getAllHDUs bs = do
    (hdu, rest) <- getOneHDU bs
    case BS.length rest < hduBlockSize of
      True  -> return [hdu]
      False -> return [hdu] -- fmap ([hdu] ++) $ getAllHDUs rest

getOneHDU :: ByteString -> IO (HeaderDataUnit, ByteString)
getOneHDU bs =
    case isAscii header of
      False -> error "Header data is not ASCII. Please Check your input file and try again"
      True  -> do
        case M.runParser headerBlockParse "FITS" (TE.decodeUtf8 header) of
          Right mainHeader -> do
            let (dataUnit, remainder) = BS.splitAt (fromIntegral $ dataSize mainHeader) rest
            return (HeaderDataUnit mainHeader dataUnit, remainder)
          Left e -> let err = M.errorBundlePretty e in error err
  where
    (header, rest) = BS.splitAt hduBlockSize bs

dataSize :: HeaderData -> Natural
dataSize h = paddedsize
  where
    axesCount = length $ axes h
    wordCount  = foldr (*) 1 $ map axisElementCount $ axes h
    wordsize = fromIntegral . bitPixToWordSize $ bitPixFormat h
    datasize = wordsize * wordCount
    padding = if axesCount == 0 then 0 else fromIntegral hduBlockSize - (datasize `mod` (fromIntegral hduBlockSize))
    paddedsize = fromIntegral (datasize + padding)
