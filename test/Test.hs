{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test
  ( Test
  , runTests
  , describe
  , it
  , testMain
  , module Test.Tasty
  , module Test.Tasty.HUnit
  , liftIO, MonadIO
  , forM_
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Writer
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)


-- Test monad with describe/it
newtype Test a = Test {runTest :: WriterT [TestTree] IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadWriter [TestTree])

runTests :: TestName -> Test () -> IO TestTree
runTests n (Test t) = do
    tests <- execWriterT t :: IO [TestTree]
    return $ testGroup n tests


describe :: TestName -> Test () -> Test ()
describe n t = do
    ts <- liftIO $ runTests n t
    tell [ts]

it :: TestName -> IO () -> Test ()
it n a = do
    tell [testCase n a]


testMain :: IO TestTree -> IO ()
testMain mtt = do
    tt <- mtt
    defaultMain tt



