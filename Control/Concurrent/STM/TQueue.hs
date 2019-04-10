{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TQueue
-- Copyright   :  (c) The University of Glasgow 2012
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- A 'TQueue' is like a 'TChan', with two important differences:
--
--  * it has faster throughput than both 'TChan' and 'Chan' (although
--    the costs are amortised, so the cost of individual operations
--    can vary a lot).
--
--  * it does /not/ provide equivalents of the 'dupTChan' and
--    'cloneTChan' operations.
--
-- The implementation is based on the traditional purely-functional
-- queue representation that uses two lists to obtain amortised /O(1)/
-- enqueue and dequeue operations.
--
-- @since 2.4
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TQueue (
        -- * TQueue
        TQueue,
        newTQueue,
        newTQueueIO,
        readTQueue,
        tryReadTQueue,
        flushTQueue,
        peekTQueue,
        tryPeekTQueue,
        writeTQueue,
        unGetTQueue,
        isEmptyTQueue,
  ) where

import GHC.Conc
import Control.Monad (unless)
import Data.Typeable (Typeable)

data End a = End !Int [a]

-- | 'TQueue' is an abstract type representing an unbounded FIFO channel.
--
-- @since 2.4
data TQueue a = TQueue {-# UNPACK #-} !(TVar Int)
                       {-# UNPACK #-} !(TVar (End a))
                       {-# UNPACK #-} !(TVar (End a))
  deriving Typeable

instance Eq (TQueue a) where
  TQueue a _ _ == TQueue b _ _ = a == b

-- |Build and returns a new instance of 'TQueue'
newTQueue :: STM (TQueue a)
newTQueue = do
  old_len <- newTVar 0
  read  <- newTVar (End 0 [])
  write <- newTVar (End 0 [])
  return (TQueue old_len read write)

-- |@IO@ version of 'newTQueue'.  This is useful for creating top-level
-- 'TQueue's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTQueueIO :: IO (TQueue a)
newTQueueIO = do
  old_len <- newTVarIO 0
  read  <- newTVarIO (End 0 [])
  write <- newTVarIO (End 0 [])
  return (TQueue old_len read write)

-- |Write a value to a 'TQueue'.
writeTQueue :: TQueue a -> a -> STM ()
writeTQueue (TQueue old_len read write) a = do
  ol <- readTVar old_len
  End write_count listend <- readTVar write
  let write_count' = write_count + 1
  if 2 * write_count' >= ol
    then do
      End read_count front <- readTVar read
      let !len = ol + write_count' - read_count
      writeTVar old_len len
      writeTVar read (End 0 (front ++ reverse listend ++ [a]))
      writeTVar write (End 0 [])
    else writeTVar write (End write_count' (a:listend))

-- |Read the next value from the 'TQueue'.
readTQueue :: TQueue a -> STM a
readTQueue (TQueue old_len read write) = do
  ol <- readTVar old_len
  End read_count front <- readTVar read
  case front of
    [] -> retry
    (a:as) -> do
      let read_count' = read_count + 1
      if 2 * read_count' >= ol
        then do
          End write_count listend <- readTVar write
          let !len = ol + write_count - read_count'
          writeTVar old_len len
          writeTVar read  (End 0 (as ++ reverse listend))
          writeTVar write (End 0 [])
        else do
          writeTVar read (End read_count' as)
      return a

-- | A version of 'readTQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadTQueue :: TQueue a -> STM (Maybe a)
tryReadTQueue c = fmap Just (readTQueue c) `orElse` return Nothing

-- | Efficiently read the entire contents of a 'TQueue' into a list. This
-- function never retries.
--
-- @since 2.4.5
flushTQueue :: TQueue a -> STM [a]
flushTQueue (TQueue old_len read write) = do
  End read_count xs <- readTVar read
  End write_count ys <- readTVar write
  unless (read_count == 0 && null xs) $ writeTVar read (End 0 [])
  unless (write_count == 0 && null ys) $ writeTVar write (End 0 [])
  writeTVar old_len 0
  return (xs ++ reverse ys)

-- | Get the next value from the @TQueue@ without removing it,
-- retrying if the channel is empty.
peekTQueue :: TQueue a -> STM a
peekTQueue (TQueue _old_len read _write) = do
  End _ xs <- readTVar read
  case xs of
    x:_ -> return x
    [] -> retry

-- | A version of 'peekTQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryPeekTQueue :: TQueue a -> STM (Maybe a)
tryPeekTQueue (TQueue _old_len read _write) = do
  End _ xs <- readTVar read
  case xs of
    x:_ -> return (Just x)
    [] -> return Nothing

-- |Put a data item back onto a channel, where it will be the next item read.
unGetTQueue :: TQueue a -> a -> STM ()
unGetTQueue (TQueue _old_len read _write) a = do
  End read_count xs <- readTVar read
  writeTVar read (End (read_count - 1) (a:xs))

-- |Returns 'True' if the supplied 'TQueue' is empty.
isEmptyTQueue :: TQueue a -> STM Bool
isEmptyTQueue (TQueue _old_len read _write) = do
  End _ xs <- readTVar read
  return $! null xs
