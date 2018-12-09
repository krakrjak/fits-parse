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
import qualified Text.Megaparsec.Byte.Lexer as MBL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- explicit imports
import Numeric.Natural ( Natural )
import Data.ByteString ( ByteString )
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Void ( Void )
import Data.Default ( def )
import Text.Ascii ( isAscii )
import Text.Megaparsec ( Parsec, ParseErrorBundle, (<?>))

-- full module imports
---- parser-combinators
import Control.Applicative.Combinators
import Control.Applicative.Permutations

-- local imports
import Data.Fits

type Parser = Parsec Void ByteString
type ParseErr = ParseErrorBundle Word8 Void

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
    (simple, bitpix, axesDesc, bZero, bScale, pCreator, pDate) <- runPermutation $ 
        (,,,,,,) <$> toPermutation (parseSimple <?> "simple")
               <*> toPermutation (parseBitPix <?> "bitpix")
               <*> toPermutation ((parseAxisCount >>= parseNaxes) <?> "axis parsing")
               <*> toPermutationWithDefault 0 (parseBzero <?> "bzero")
               <*> toPermutationWithDefault 0 (parseBscale <?> "bscale")
               <*> toPermutationWithDefault (StringValue NullString Nothing) (parseCreator <?> "creator")
               <*> toPermutationWithDefault (StringValue NullString Nothing) (parseDate <?> "date")
    parseEnd >> return defHeader { simpleFormat = simple
                     , bitPixFormat = bitpix
                     , nAxisMetaData = parsedNAxisMeta (length axesDesc)
                     , axes = axesDesc
                     , observationDate = pDate
                     , authorIdentifier = pCreator }
  where
    defHeader = def :: HeaderData
    defNAxisMeta = def :: NAxisMetadata
    parsedNAxisMeta n | n == 0 = defNAxisMeta { naxisType = ZeroAxes
                                              , axesCount = fromIntegral n }
    parsedNAxisMeta n          = defNAxisMeta { naxisType = ManyAxes
                                              , axesCount = fromIntegral n }

parseSimple :: Parser SimpleFormat
parseSimple = M.string' "simple" >> parseEquals
    >> ((conformParse >> consumeDead >> return Conformant)
    <|> (nonConformParse >> consumeDead >> return NonConformant))
  where
    conformParse = M.single (BS.c2w 't')
    nonConformParse = M.anySingle

parseEquals :: Parser ()
parseEquals = M.space >> M.single (BS.c2w '=') >> M.space

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
skipEmpty = (M.many $ M.satisfy ((0::Word8) ==)) >> return ()

consumeDead :: Parser ()
consumeDead = M.space >> skipEmpty

parseEnd :: Parser ()
parseEnd = M.string' "end" >> consumeDead >> M.takeRest >> return ()

parseNatural :: Parser Natural
parseNatural = do
    v <- MBL.decimal
    consumeDead
    return $ fromIntegral v

parseInteger :: Parser Int
parseInteger = do
    v <- MBL.decimal
    consumeDead
    return v

parseStringValue :: Parser StringValue
parseStringValue = do 
       -- The rules are weird, NULL means a NULL string, '' is an empty
       -- string, a ' followed by a bunch of spaces and a close ' is
       -- considered an empty string, and trailing whitespace is ignored
       -- within the quotes, but not leading spaces. 
    v <- (M.try parseInQuotes) <|> return []
    let bs = BS.pack v
    if BS.length bs < 1
        then consumeDead >> return (StringValue NullString Nothing)
        else consumeDead >> return (StringValue DataString (Just $ TE.decodeUtf8 bs))

parseInQuotes :: Parser [Word8]
parseInQuotes = between (M.single 39) (M.single 39) $ M.many M.anySingle

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
          Left e -> let err = M.errorBundlePretty e in print header >> error err
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
