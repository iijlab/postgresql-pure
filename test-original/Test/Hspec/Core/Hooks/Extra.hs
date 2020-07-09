module Test.Hspec.Core.Hooks.Extra
  ( beforeAllWith
  ) where

import           Test.Hspec.Core.Hooks
import           Test.Hspec.Core.Spec

import           Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import           Control.Exception       (SomeException, throwIO, try)

data Memoized a =
    Empty
  | Memoized a
  | Failed SomeException

memoize :: MVar (Memoized a) -> IO a -> IO a
memoize mvar action = do
  result <- modifyMVar mvar $ \ma -> case ma of
    Empty -> do
      a <- try action
      return (either Failed Memoized a, a)
    Memoized a -> return (ma, Right a)
    Failed _ -> throwIO (Pending Nothing (Just "exception in beforeAll-hook (see previous failure)"))
  either throwIO return result

-- | Run a custom action befor the first spec item.
beforeAllWith :: (b -> IO a) -> SpecWith a -> SpecWith b
beforeAllWith action spec = do
  mver <- runIO (newMVar Empty)
  beforeWith (memoize mver . action) spec
