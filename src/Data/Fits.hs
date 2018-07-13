{-|
Module      : HeaderDataUnit
Description : Types for FITS Data Units
Copyright   : (c) Zac Slade, 2018
License     : BSD2
Maintainer  : krakrjak@gmail.com
Stability   : experimental

Definitions for the data types needed to parse an HDU in a FITS block.
-}

{-# LANGUAGE OverloadedStrings #-}
module Data.Fits
    ( -- * Main data types
      HeaderDataUnit(..)
    , HeaderData(..)

      -- ** Helper data types
    , SimpleFormat(..)
    , BitPixFormat(..)
    , StringType(..)
    , StringValue(..)
    , NumberType(..)
    , NumberModifier(..)
    , NumberValue(..)
    , NAxisMetadata(..)
    , Axis(..)

      -- * Utility
    , bitPixToWordSize
    , hduRecordLength
    , hduMaxRecords
    , hduBlockSize
    ) where

import Numeric.Natural ( Natural )
import Data.Text ( Text )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS

import Data.Default( Default, def )

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
                | UndefinedString -- ^ (e.g. AUTHOR is not in the header)

-- | A 'StringValue' is a type paired with a possible value.
data StringValue = StringValue
    { stringType :: StringType  -- ^ Which 'StringType' is this value?
    , stringValue :: Maybe Text -- ^ The payload of a character string
    }

{-| The default instance of 'StringValue' is an undefined string with no
    text.
-}
instance Default StringValue where
    def = StringValue UndefinedString Nothing

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

{-| An HDU can be simply a header with no data, or it can have axes
    defined. 'NAxisType' sums this up nicely with 'ZeroAxes' for header only
    data and 'ManyAxes' when there is some data  in the array.
-}
data NAxisType = ZeroAxes -- ^ No data follows the header.
               | ManyAxes -- ^ There is data in this supposed FITS file.

{-| If we have 'ManyAxes', how many do we have. That's the question
   'NAxisMetadata' is here to answer. The 'axesCount' record corresponds
   with how many NAXISN records there are in the header. If 'axesCount' is
   three then the header will contain 3 NAXISN records, NAXIS1, NAXIS2, and
   NAXIS3.
-}
data NAxisMetadata = NAxisMetadata
    { naxisType :: NAxisType -- ^ Are there zero or more axes of data?
    , axesCount :: Int       -- ^ How many axes are there in the dataset?
    }

{-| The default instance of 'NAxisMetadata' has 'ZeroAxes' and the count
    is set to 0.
-}
instance Default NAxisMetadata where
    def = NAxisMetadata ZeroAxes 0

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

{-| The header part of the HDU is vital carrying not only authorship
    metadata, but also specifying how to make sense of the binary payload
    that starts 2,880 bytes after the start of the 'HeaderData'.
-}
data HeaderData = HeaderData
    { simpleFormat :: SimpleFormat
      -- ^
    , bitPixFormat :: BitPixFormat
      -- ^
    , nAxisMetaData :: NAxisMetadata
      -- ^
    , axes :: [Axis]
      -- ^
    , objectIdentifier :: StringValue
      -- ^
    , observationDate :: StringValue
      -- ^
    , originIdentifier :: StringValue
      -- ^
    , telescopeIdentifier :: StringValue
      -- ^
    , instrumentIdentifier :: StringValue
      -- ^
    , observerIdentifier :: StringValue
      -- ^
    , authorIdentifier :: StringValue
      -- ^
    , referenceString :: StringValue
      -- ^
    }

instance Default HeaderData where
    def = HeaderData Conformant ThirtyTwoBitInt (def :: NAxisMetadata) []
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