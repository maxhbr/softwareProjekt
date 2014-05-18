-- | Simple in-process key/value cache
--
-- from: https://gist.github.com/singpolyma/6029835
--
-- Of course, for really simple stuff you could probably use unsafeInterleaveIO
module Stolen.Cache (Cache, newCache, fromCache) where

import Control.Monad (void)

-- Could also use STM instead
import Control.Concurrent (forkIO, Chan, newChan, readChan, writeChan, MVar, newEmptyMVar, putMVar, takeMVar)

import qualified Data.Map as Map

data CacheMsg k v = CacheMsg k (IO v) (MVar v)

-- | A single cache
newtype Cache k v = Cache (Chan (CacheMsg k v))

cacheThread :: (Ord k) => Chan (CacheMsg k v) -> IO ()
cacheThread chan = next Map.empty
  where
  next m = readChan chan >>= go m

  go m (CacheMsg k io reply) = case Map.lookup k m of
    Just v -> putMVar reply v >> next m
    Nothing -> do
      v <- io
      putMVar reply v
      next (Map.insert k v m)

-- | Create a new cache
newCache :: (Ord k) => IO (Cache k v)
newCache = do
  chan <- newChan
  -- This cache thread never terminates, so this is for a process-life cache
  -- That would be easy to change, but I won't bother here
  void $ forkIO (cacheThread chan)
  return (Cache chan)

syncCall :: Chan a -> (MVar r -> a) -> IO r
syncCall chan msg = do
  r <- newEmptyMVar
  writeChan chan (msg r)
  takeMVar r

-- | Get a value from the cache, or compute it and cache it
fromCache :: Cache k v -> k -> IO v -> IO v
fromCache (Cache chan) k io = syncCall chan (CacheMsg k io)
