module RepeatThreadPool
  ( Pool
  , start
  , state
  , kill
  ) where

import           Control.Concurrent (ThreadId, forkIO, killThread)
import           Control.Monad      (replicateM)
import           Data.IORef         (IORef, newIORef, readIORef, writeIORef)
import           Data.Traversable   (for)

newtype Pool a = Pool [(ThreadId, IORef a)]

start :: Word -> IO a -> (a -> IO a) -> IO (Pool a)
start n ini task = do
  ts <- replicateM (fromIntegral n) $ do
    s <- ini
    ref <- newIORef s
    tid <- forkIO $ go ref s
    pure (tid, ref)
  pure $ Pool ts
  where
    go r s = do
      s' <- task s
      writeIORef r s'
      go r s'

state :: Pool a -> IO [a]
state (Pool ts) = for ts $ \(_, ref) -> readIORef ref

kill :: Pool a -> IO [a]
kill (Pool ts) =
  for ts $ \(tid, ref) -> do
    killThread tid
    readIORef ref
