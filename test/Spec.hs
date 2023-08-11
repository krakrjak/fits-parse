module Main where

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "Hello"
      [ testCase "TESTING" $ do
          print "EHLLO"
          error "FAILED"
      ]
