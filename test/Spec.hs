{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Writer
import qualified Data.ByteString as BS
import Data.Fits.MegaParser
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

main :: IO ()
main =
  defaultMain $ runTests "Tests" $ do
    basicParsing
    sampleSpiral
    sampleNSO

basicParsing :: Test ()
basicParsing = describe "Basic Parsing" $ do
  it "should parse simple bzero header" $ do
    let res = M.runParser parseBzero "Test" "bzero=3"
    res @?= Right 3

sampleSpiral :: Test ()
sampleSpiral =
  describe "Spiral Sample FITS Parse" $ do
    it "should parse" $ do
      bs <- BS.readFile "./fits_files/Spiral_2_30_0_300_10_0_NoGrad.fits"
      hdus <- getAllHDUs bs
      length hdus @?= 1

sampleNSO :: Test ()
sampleNSO = do
  describe "NSO Sample FITS Parse" $ do
    it "should parse" $ do
      bs <- BS.readFile "./fits_files/nso_dkist.fits"
      hdus <- getAllHDUs bs
      length hdus @?= 1

-- Test monad with describe/it
newtype Test a = Test {runTest :: Writer [TestTree] a}
  deriving (Functor, Applicative, Monad, MonadWriter [TestTree])

runTests :: TestName -> Test () -> TestTree
runTests n (Test t) =
  let tests = execWriter t :: [TestTree]
   in testGroup n tests

describe :: TestName -> Test () -> Test ()
describe n t =
  tell [runTests n t]

it :: TestName -> IO () -> Test ()
it n a = do
  tell [testCase n a]
