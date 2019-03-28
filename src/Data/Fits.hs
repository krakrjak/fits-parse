{-|
Module      : Data.Fits
Description : Types for FITS Data Units
Copyright   : (c) Zac Slade, 2018
License     : BSD2
Maintainer  : krakrjak@gmail.com
Stability   : experimental

Definitions for the data types needed to parse an HDU in a FITS block.
-}

{-# LANGUAGE PartialTypeSignatures, DataKinds, ExistentialQuantification
  , ScopedTypeVariables, GADTs
  , OverloadedStrings, TypeOperators, TypeFamilies #-}
module Data.Fits
    ( -- * Data payload functions
      parsePix
    , pixsUnwrapI
    , pixsUnwrapD

      -- * Main data types
    , HeaderDataUnit(..)
    , HeaderData(..)
    , BitPixFormat(..)

      -- ** Header Data Types
    , SimpleFormat(..)
    , Axis(..)
    , StringType(..)
    , StringValue(..)
    , NumberType(..)
    , NumberModifier(..)
    , NumberValue(..)

      -- * Utility
    , isBitPixInt
    , isBitPixFloat
    , bitPixToWordSize

      -- ** Constants
    , hduRecordLength
    , hduMaxRecords
    , hduBlockSize

    ) where

---- text
import qualified Data.Text as T
---- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

---- base
import Numeric.Natural ( Natural )

---- ghc
import GHC.TypeNats (KnownNat, Nat)

---- text
import Data.Text ( Text )

---- bytestring
import Data.ByteString ( ByteString )

---- default
import Data.Default( Default, def )

---- binaryi
import Data.Binary
import Data.Binary.Get

-- | A single record in the HDU is an eighty byte word.
{-@ type HDURecordLength = {v:Int | v = 80} @-}
{-@ hduRecordLength :: HDURecordLength @-}
hduRecordLength :: Int
hduRecordLength = 80

{-| The maximum amount of eighty byte records is thirty-six per the
    standard.
-}
{-@ type HDUMaxRecords = {v:Int | v = 36} @-}
{-@ hduMaxRecords :: HDUMaxRecords @-}
hduMaxRecords :: Int
hduMaxRecords = 36

{-| The size of an HDU block is fixed at thirty-six eighty byte words. In
    other words 2,880 bytes. These blocks are padded with zeros to this
    boundary.
-}
{-@ type HDUBlockSize = {v:Int | v = 2880} @-}
{-@ hduBlockSize :: HDUBlockSize @-}
hduBlockSize :: Int
hduBlockSize = hduRecordLength * hduMaxRecords
 
{-| There are many types of strings defined in the FITS documentation.
    Refered to as "character string(s)" in the documentation, they can be
    null, empty, undefined, or contain characters (printable ASCII only).
-} 
data StringType = NullString      -- ^ The NULL character string (e.g. AUTHOR=)
                | EmptyString     -- ^ An empty string (e.g. AUTHOR="")
                | DataString      -- ^ Plain ASCII data

instance Show StringType where
    show NullString      = "Null String"
    show EmptyString     = "Empty quoted String"
    show DataString      = "String"

-- | A 'StringValue' is a type paired with a possible value.
data StringValue = StringValue
    { stringType :: StringType  -- ^ Which 'StringType' is this value?
    , stringValue :: Maybe Text -- ^ The payload of a character string
    }

{-| The default instance of 'StringValue' is an undefined string with no
    text.
-}
instance Default StringValue where
    def = StringValue NullString Nothing

instance Show StringValue where
        show (StringValue NullString _) = show NullString
        show (StringValue EmptyString _) = show EmptyString
        show (StringValue DataString Nothing) = "No good " ++ show DataString
        show (StringValue DataString (Just s)) = show DataString ++ T.unpack s

{-| The FITS standard allows for the encoding of unsigned integers, signed
    integers, real numbers, and complex numbers. They are always ASCII
    encoded. See 5.2 of the standard for more details.
-}
data NumberType =
      IntegerType -- ^ HDU ASCII encoded integer number
    | RealType    -- ^ HDU ASCII encoded real number
    | ComplexType -- ^ HDU ASCII encoded complex number

-- | Utility data type to help with the ASCII representation of numbers
data NumberModifier =
      Positive -- ^ HDU positive number value
    | Negative -- ^ HDU negative number value
    | Zero     -- ^ HDU numeric value is zero, could be positive or negative

{-| 'NumberValue' contains an encoded numeric record from a data field.
  This data type still needs to be converted into more useful Haskell data
  types.
-}
data NumberValue = NumberValue
    { numberType        :: NumberType
      -- ^ Key to decoding the structure
    , realModifier      :: NumberModifier
      -- ^ Encoding the sign of the real part
    , realPart          :: Text
      -- ^ All 'NumberType' have a real part (sign stripped)
    , imaginaryModifier :: Maybe NumberModifier
      -- ^ Encoding the sign of the imaginary part
    , imaginaryPart     :: Maybe Text
      -- ^ Only 'ComplexType' have an imaginary part (sign stripped)
    , exponentModifier  :: Maybe NumberModifier
      -- ^ 'Positive', 'Negative', or 'Zero'
    , exponent          :: Maybe Int
      -- ^ All 'NumberType' may have an exponent
    }

{-| The default instance for 'NumberValue' is a 32-bit integer of zero with
    no imaginary or exponent parts.
-}
instance Default NumberValue where
    def = NumberValue IntegerType Zero "0" Nothing Nothing Nothing Nothing

{-| The standard defines two possible values for the SIMPLE keyword, T and
    F. The T refers to a 'Conformant' format while F refers to
    a 'NonConformant' format. At this time only the 'Conformant', T, format
    is supported.
-}
data SimpleFormat = Conformant
                    -- ^ Value of SIMPLE=T in the header. /supported/
                  | NonConformant
                    -- ^ Value of SIMPLE=F in the header. /unsupported/

-- | 'Axis' represents a single NAXIS record.
data Axis = Axis
    { axisNumber       :: Int -- ^ The axis number under consideration
    , axisElementCount :: Int -- ^ The number of elements in this axis
    }

-- | The default instance for 'Axis' is NAXIS=0 with zero elements.
instance Default Axis where
    def = Axis 0 0

{-| The 'BitPixFormat' is the nitty gritty of how the 'Axis' data is layed
    out in the file. The standard recognizes six formats: unsigned 8 bit
    integer, two's complement binary integers at 16, 32, and 64 bits along
    with 32 and 64 bit IEEE floating point formats.
-}
data BitPixFormat =
      EightBitInt       -- ^ BITPIX = 8; unsigned binary integer of 8 bits
    | SixteenBitInt     -- ^ BITPIX = 16; two's complement binary integer of 16 bits
    | ThirtyTwoBitInt   -- ^ BITPIX = 32; two's complement binary integer of 32 bits
    | SixtyFourBitInt   -- ^ BITPIX = 64; two's complement binary integer of 64 bits
    | ThirtyTwoBitFloat -- ^ BITPIX = -32; IEEE single precision floating point of 32 bits
    | SixtyFourBitFloat -- ^ BITPIX = -64; IEEE double precision floating point of 64 bits

instance Show BitPixFormat where
        show EightBitInt       = "8 bit unsigned integer"
        show SixteenBitInt     = "16 bit signed integer"
        show ThirtyTwoBitInt   = "32 bit signed integer"
        show SixtyFourBitInt   = "64 bit signed interger"
        show ThirtyTwoBitFloat = "32 bit IEEE single precision float"
        show SixtyFourBitFloat = "64 bit IEEE double precision float"

{-| This utility function can be used to get the word count for data in an
    HDU.
-}
bitPixToWordSize :: BitPixFormat -> Natural
bitPixToWordSize EightBitInt       = 8
bitPixToWordSize SixteenBitInt     = 16
bitPixToWordSize ThirtyTwoBitInt   = 32
bitPixToWordSize ThirtyTwoBitFloat = 32
bitPixToWordSize SixtyFourBitInt   = 64
bitPixToWordSize SixtyFourBitFloat = 64

{-| This utility function can be used to get the size in bytes of the
-   format.
-}
bitPixToByteSize :: BitPixFormat -> Natural
bitPixToByteSize EightBitInt       = 1
bitPixToByteSize SixteenBitInt     = 2
bitPixToByteSize ThirtyTwoBitInt   = 4
bitPixToByteSize ThirtyTwoBitFloat = 4
bitPixToByteSize SixtyFourBitInt   = 8
bitPixToByteSize SixtyFourBitFloat = 8

{- | This utility functions quickly lets you know if you are dealing with
     integer data.
-}
isBitPixInt :: BitPixFormat -> Bool
isBitPixInt EightBitInt     = True
isBitPixInt SixteenBitInt   = True
isBitPixInt ThirtyTwoBitInt = True
isBitPixInt SixtyFourBitInt = True
isBitPixInt _ = False

{- | This utility functions quickly lets you know if you are dealing with
     floating point data.
-}
isBitPixFloat :: BitPixFormat -> Bool
isBitPixFloat ThirtyTwoBitFloat = True
isBitPixFloat SixtyFourBitFloat = True
isBitPixFloat _ = False

{- | Following `BitPixFormat` we have a tag for integer and floating point
     values. We box them up to ease parsing.
-}
data Pix = PB Int | PI16 Int | PI32 Int | PI64 Int | PF Double | PD Double

{- | Removes the `Pix` tag from an `Int` type within. -}
unPixI :: Pix -> Int
unPixI (PB b)   = b
unPixI (PI16 i) = i
unPixI (PI32 i) = i
unPixI (PI64 i) = i
unPixI _        = error "Pix are not stored as integers, invalid unpacking"

{- | Removes the `Pix` tag from a `Double` type within. -}
unPixD :: Pix -> Double
unPixD (PF d)   = d
unPixD (PD d)   = d
unPixD _        = error "Pix are not stored as floating point values, invalid unpacking"

{- | Remove the Pix wrapper for integer `Pix` lists.  -}
pixsUnwrapI :: BitPixFormat -> [Pix] -> [Int]
pixsUnwrapI EightBitInt       pxs = map unPixI pxs
pixsUnwrapI SixteenBitInt     pxs = map unPixI pxs
pixsUnwrapI ThirtyTwoBitInt   pxs = map unPixI pxs
pixsUnwrapI SixtyFourBitInt   pxs = map unPixI pxs
pixsUnwrapI _ _ = error "BitPixFormat is not an integer type"

{- | Remove the `Pix` wrapper for floating point `Pix` lists.  -}
pixsUnwrapD :: BitPixFormat -> [Pix] -> [Double]
pixsUnwrapD ThirtyTwoBitFloat pxs = map unPixD pxs
pixsUnwrapD SixtyFourBitFloat pxs = map unPixD pxs
pixsUnwrapD _ _ = error "BitPixFormat is not a floating point type"

getPix :: BitPixFormat -> Get Pix
getPix EightBitInt       = PB . fromIntegral <$> getInt8
getPix SixteenBitInt     = PI16 . fromIntegral <$> getInt16be
getPix ThirtyTwoBitInt   = PI32 . fromIntegral <$> getInt32be
getPix SixtyFourBitInt   = PI64 . fromIntegral <$> getInt64be
getPix ThirtyTwoBitFloat = PF . realToFrac <$> getFloatbe
getPix SixtyFourBitFloat = PD . realToFrac <$> getDoublebe

getPixs :: Int -> BitPixFormat -> Get [Pix]
getPixs c bpf = do
    empty <- isEmpty
    if empty
      then return []
      else do
        p <- getPix bpf
        ps <- getPixs (c - 1) bpf
        return (p:ps)

{- | This is the main low-level function which parses the data portion of an
     HDU. You need and element count, a format and a bytestring. The resulting
     list is produced in column-row major order as specified in the standard.
-}
parsePix :: Int -> BitPixFormat -> BL.ByteString -> IO [Pix]
parsePix c bpf bs = return $ runGet (getPixs c bpf) bs

{- `pixDimsByCol` takes a list of Axis and gives a column-row major list of
    axes dimensions.
-}
pixDimsByCol :: [Axis] -> [Int]
pixDimsByCol = map axisElementCount

{- `pixDimsByRow` takes a list of Axis and gives a row-column major list of
    axes dimensions.
-}
pixDimsByRow :: [Axis] -> [Int]
pixDimsByRow = reverse . pixDimsByCol

{-| The header part of the HDU is vital carrying not only authorship
    metadata, but also specifying how to make sense of the binary payload
    that starts 2,880 bytes after the start of the 'HeaderData'.
-}

data HeaderData = HeaderData
    { simpleFormat :: SimpleFormat
      -- ^ SIMPLE
    , bitPixFormat :: BitPixFormat
      -- ^ BITPIX
    , axes :: [Axis]
      -- ^ Axes metadata
    , objectIdentifier :: StringValue
      -- ^ OBJECT
    , observationDate :: StringValue
      -- ^ DATE
    , originIdentifier :: StringValue
      -- ^ OBJECT
    , telescopeIdentifier :: StringValue
      -- ^ TELESCOP
    , instrumentIdentifier :: StringValue
      -- ^ INSTRUME
    , observerIdentifier :: StringValue
      -- ^ OBSERVER
    , authorIdentifier :: StringValue
      -- ^ CREATOR
    , referenceString :: StringValue
      -- ^ REFERENC
    }

instance Default HeaderData where
    def = HeaderData NonConformant EightBitInt []
        (def :: StringValue) (def :: StringValue) (def :: StringValue)
        (def :: StringValue) (def :: StringValue) (def :: StringValue)
        (def :: StringValue) (def :: StringValue)

{-| The 'HeaderDataUnit' is the full HDU. Both the header information is
    encoded alongside the 'Axis' payload.
-}
data HeaderDataUnit = HeaderDataUnit
    { headerData :: HeaderData
      -- ^ Just the header part of the HDU
    , payloadData :: ByteString
      -- ^ The actual data payload
    }
