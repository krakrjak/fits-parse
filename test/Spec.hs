module Main where

import Test
import SpecParse
import SpecArray

main :: IO ()
main = testMain $ runTests "FITS" $ do
  describe "Parse" SpecParse.tests
  describe "Array" SpecArray.tests
