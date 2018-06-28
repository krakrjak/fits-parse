{-|
Module      : HeaderDataUnit
Description : Types for FITS Data Units
Copyright   : (c) Zac Slade, 2018
License     : BSD2
Maintainer  : krakrjak@gmail.com
Stability   : experimental

Definitions for the data types needed to parse an HDU in a FITS block.
-}

module HeaderDataUnit where

import Data.Text (Text)

type Bytes = Int
type Count = Int

{-@ type HDURecordLength = {v:Int | v = 80} @-}
{-@ hduRecordLength :: HDURecordLength @-}
hduRecordLength :: Bytes
hduRecordLength = 80

{-@ type HDUMaxRecords = {v:Int | v = 36} @-}
{-@ hduMaxRecords :: HDUMaxRecords @-}
hduMaxRecords :: Count
hduMaxRecords = 36

{-@ type HDUBlockSize = {v:Int | v = 2880} @-}
{-@ hduBlockSize :: HDUBlockSize @-}
hduBlockSize :: Bytes
hduBlockSize = hduRecordLength * hduMaxRecords

data StringType = NullString | EmptyString | DataString | UndefinedString

data StringValue = StringValue
    { stringType :: StringType
    , stringValue :: Maybe Text
    }

data NumberType =
      IntegerType -- ^ HDU ASCII encoded integer number
    | RealType    -- ^ HDU ASCII encoded real number
    | ComplexType -- ^ HDU ASCII encoded complex number

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
    , realModifier      :: NumberModifier
    , realPart          :: Text
    , imaginaryModifier :: Maybe NumberModifier
    , imaginaryPart     :: Maybe Text
    , exponentModifier  :: Maybe NumberModifier
    , exponent          :: Maybe Int
    }

data SimpleFormat = Conformant    -- ^ Value of SIMPLE is T in the header
                  | NonConformant -- ^ Value of SIMPLE is F in the header

data NAxisType = ZeroAxes -- ^ No data follows the header
               | ManyAxes -- ^ There is one or more axes in the data following the header

data NAxisMetadata = NAxisMetadata
    { naxisType :: NAxisType -- ^ Are there zero or more axes of data?
    , axesCount :: Int       -- ^ How many axes are there in the dataset?
    }

data Axis = Axis
    { axisNumber       :: Int -- ^ The axis number under consideration
    , axisElementCount :: Int -- ^ The number of elements in this axis
    }

data BitPixFormat =
      EightBitInt       -- ^ BITPIX = 8; unsigned binary integer of 8 bits
    | SixteenBitInt     -- ^ BITPIX = 16; two's complement binary integer of 16 bits
    | ThirtyTwoBitInt   -- ^ BITPIX = 32; two's complement binary integer of 32 bits
    | SixtyFourBitInt   -- ^ BITPIX = 64; two's complement binary integer of 64 bits
    | ThirtyTwoBitFloat -- ^ BITPIX = -32; IEEE single precision floating point of 32 bits
    | SixtyFourBitFloat -- ^ BITPIX = -64; IEEE double precision floating point of 64 bits


