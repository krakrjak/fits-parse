{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
module SpecArray where

import Test
import Data.Fits.Array
import Data.Massiv.Array
import qualified Data.ByteString.Lazy as BL
import Data.Fits (BitPixFormat(..), Axes)
import Data.Word (Word8)

tests :: Test ()
tests = do
  describe "indices" testIndices
  describe "decode vector" testDecode
  describe "decode array" testDecodeArray


testIndices :: Test ()
testIndices = do
  it "Ix1" $ do
    ix <- axesIndex $ rowMajorAxes [10]
    ix @?= Ix1 10

  it "Ix2 in reverse order" $ do
    ix <- axesIndex $ rowMajorAxes [3, 2]
    ix @?= 2 :. 3

  it "Ix3 in reverse order" $ do
    ix <- axesIndex $ rowMajorAxes [3, 2, 1]
    ix @?= 1 :> 2 :. 3


genInput :: Word8 -> BL.ByteString
genInput start = BL.pack [start..start+100]


testDecode :: Test ()
testDecode = do
  let input = genInput 0

  it "should calc total pixels" $ do
    totalPix [3, 2, 2] @?= 12

  it "should get pixels" $ do
    px <- runGetThrow (getAxesVector (getPix EightBitInt) [5]) input
    let arr = compute px :: Array P Ix1 Int
    arr @?= [0,1,2,3,4]

  it "should get pixels 2d" $ do
    px <- runGetThrow (getAxesVector (getPix EightBitInt) [3, 2]) input
    let arr = compute px :: Array P Ix1 Int
    arr @?= [0,1,2,3,4,5]


testDecodeArray :: Test ()
testDecodeArray = do
  let input = genInput 0
  it "should decode Ix1" $ do
    a <- decodeArray' @Ix1 @Int EightBitInt [3] input
    a @?= [0,1,2]

  it "should decode Ix2" $ do
    a <- decodeArray' @Ix2 @Int EightBitInt [3, 2] input
    a @?= [[0,1,2],[3,4,5]]

  it "should decode Ix3" $ do
    a <- decodeArray' @Ix3 @Int EightBitInt [2, 2, 2] input
    a @?= [[[0,1],[2,3]],[[4,5],[6,7]]]



