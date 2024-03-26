{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
-- you'll have to export all the things if you want it to be useful...
module Data.Fits.Array
  ( decodeImage
  , decodeArray'
  , AxesIndex(..)
  , DecodePix(..)
  , getAxesVector
  , runGetThrow
  , rowMajorAxes
  , totalPix
  , Array
  , Ix1, Ix2, Ix3, Ix4, Ix5
  ) where

import Control.Monad (replicateM)
import qualified Data.Fits as Fits
import Data.Int
import Data.Fits (BitPixFormat(..), HeaderDataUnit(..), Axes(..), Axis)
import qualified Data.List as L
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.Massiv.Array as A hiding (isEmpty, product)
import Data.Massiv.Vector as V hiding (product)



-- | Decode HeaderDataUnit's main data into an Image of arbitrary dimension 'ix'
--
-- > {-# LANGUAGE TypeApplications #-}
-- > import Data.Massiv.Array
-- > import Data.Fits.Image
-- > import Data.Fits
-- >
-- > decodeExample :: BL.ByteString -> Either String Int
-- > decodeExample bs = do
-- >  hdu <- readPrimaryHDU bs
-- >  arr <- decodeImage @Ix2 hdu 
-- >  pure $ arr !> 1 ! 2
decodeImage :: (Index ix, AxesIndex ix, Prim a, DecodePix a) => HeaderDataUnit -> Either String (Array P ix a)
decodeImage hdu = do
  either (Left . show) pure $ do
    decodeArray' hdu._dimensions._bitpix hdu._dimensions._axes hdu._mainData


-- | Decode data into an Array of arbitrary dimension 'ix' using 'BitPixFormat' and 'Axes'
decodeArray' :: forall ix a m. (AxesIndex ix, Prim a, MonadThrow m, DecodePix a, Index ix) => BitPixFormat -> Axes -> BL.ByteString -> m (Array P ix a)
decodeArray' f as inp = do
  v <- runGetThrow getAll inp
  fromVector as v
  where
    getAll :: Get (Vector DS a)
    getAll = getAxesVector (getPix f) as

    fromVector :: forall ix a m. (AxesIndex ix, Index ix, MonadThrow m, Prim a) => [Axis] ->Array DS Ix1 a -> m (Array P ix a)
    fromVector as v = do
      ix <- axesIndex $ rowMajorAxes as
      let vc = compute v
      resizeM (Sz ix) vc


getAxesVector :: Get a -> Axes -> Get (Vector DS a)
getAxesVector get as = do
  sreplicateM (Sz1 (totalPix as)) get


totalPix :: Axes -> Int
totalPix as = product as


-- | Normal 'Axes' are sorted with the inner-most axis first. RowMajorAxes are the reverse, with the outer dimension first
newtype RowMajorAxes = RowMajorAxes [Axis]
  deriving (Show)

rowMajorAxes :: Axes -> RowMajorAxes
rowMajorAxes as = RowMajorAxes (L.reverse as)


class AxesIndex ix where
  axesIndex :: MonadThrow m => RowMajorAxes -> m ix

instance AxesIndex Ix1 where
  axesIndex (RowMajorAxes [i]) = pure i
  axesIndex as = throwM $ AxesMismatch as

instance AxesIndex Ix2 where
  axesIndex (RowMajorAxes [c, r]) = do
    ix1 <- axesIndex $ RowMajorAxes [r]
    pure $ c :. ix1
  axesIndex as = throwM $ AxesMismatch as

instance AxesIndex Ix3 where
  axesIndex = axesIndexNext

instance AxesIndex Ix4 where
  axesIndex = axesIndexNext

instance AxesIndex Ix5 where
  axesIndex = axesIndexNext



axesIndexNext :: AxesIndex (Lower (IxN n)) => MonadThrow m => RowMajorAxes -> m (IxN n)
axesIndexNext (RowMajorAxes (n:as)) = do
    ixl <- axesIndex (RowMajorAxes as)
    pure $ n :> ixl
axesIndexNext as = throwM $ AxesMismatch as






runGetThrow :: forall a m. MonadThrow m => Get a -> BL.ByteString -> m a
runGetThrow get inp =
  case runGetOrFail get inp of
    Left (_, bytes, e) -> throwM $ ParseError bytes e
    Right (_, _, a) -> pure a

data ParseError
  = ParseError !ByteOffset !String
  | AxesMismatch !RowMajorAxes
  deriving (Show, Exception)







-- class DecodePix a where
class DecodePix a where
  getPix :: BitPixFormat -> Get a

instance DecodePix Int8 where
  getPix EightBitInt       = getInt8
  getPix f = fail $ "Expected Int8, but format is " <> show f

instance DecodePix Int16 where
  getPix SixteenBitInt = getInt16be
  getPix f = fail $ "Expected Int16, but format is " <> show f

instance DecodePix Int32 where
  getPix ThirtyTwoBitInt = getInt32be
  getPix f = fail $ "Expected Int32, but format is " <> show f

instance DecodePix Int64 where
  getPix SixtyFourBitInt = getInt64be
  getPix f = fail $ "Expected Int64, but format is " <> show f

instance DecodePix Int where
  getPix EightBitInt = fromIntegral <$> getPix @Int8 EightBitInt
  getPix SixteenBitInt = fromIntegral <$> getPix @Int16 SixteenBitInt
  getPix ThirtyTwoBitInt = fromIntegral <$> getPix @Int32 ThirtyTwoBitInt
  getPix SixtyFourBitInt = fromIntegral <$> getPix @Int64 SixtyFourBitInt
  getPix f = fail $ "Expected Int, but format is " <> show f

instance DecodePix Float where
  getPix ThirtyTwoBitFloat = getFloatbe
  getPix f = fail $ "Expected Float, but format is " <> show f

instance DecodePix Double where
  getPix SixtyFourBitFloat = getDoublebe
  getPix f = fail $ "Expected Double, but format is " <> show f

