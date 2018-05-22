{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP, DeriveDataTypeable #-}

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
-- The implementation is based on a purely-functional queue representation that
-- uses two lists and a schedule to obtain worst-case /O(1)/
-- enqueue and dequeue operations. See Simple and Efficient Purely Functional
-- Queues and Deques, by Chris Okasaki.
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

-- | 'TQueue' is an abstract type representing an unbounded FIFO channel.
--
-- @since 2.4
data TQueue a =
  TQueue {-# UNPACK #-} !(TVar [a]) -- front
         {-# UNPACK #-} !(TVar [a]) -- rear
         {-# UNPACK #-} !(TVar [a]) -- schedule
  deriving Typeable

instance Eq (TQueue a) where
  TQueue a _ _ == TQueue b _ _ = a == b

-- |Build and returns a new instance of 'TQueue'
newTQueue :: STM (TQueue a)
newTQueue = do
  read  <- newTVar []
  write <- newTVar []
  sched <- newTVar []
  return (TQueue read write sched)

-- |@IO@ version of 'newTQueue'.  This is useful for creating top-level
-- 'TQueue's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTQueueIO :: IO (TQueue a)
newTQueueIO = do
  read  <- newTVarIO []
  write <- newTVarIO []
  sched <- newTVarIO []
  return (TQueue read write sched)

-- |Write a value to a 'TQueue'.
writeTQueue :: TQueue a -> a -> STM ()
writeTQueue (TQueue read write sched) a = do
  listend <- readTVar write
  sch <- readTVar sched
  case sch of
    _s:ch -> do
      writeTVar write (a:listend)
      writeTVar sched ch
    [] -> do
      front <- readTVar read
      let front' = rotate front (a:listend) []
      writeTVar read front'
      writeTVar write []
      writeTVar sched front'

rotate :: [a] -> [a] -> [a] -> [a]
rotate [] (r : _) as = r : as
rotate (l : ls) (r : rs) as = l : rotate ls rs (r : as)
rotate _ _ _ = error "TQueue queue invariant violated."

-- |Read the next value from the 'TQueue'.
readTQueue :: TQueue a -> STM a
readTQueue (TQueue read write sched) = do
  front <- readTVar read
  case front of
    [] -> retry
    x:front' -> do
      sch <- readTVar sched
      case sch of
        _s:ch -> do
           writeTVar sched ch
           writeTVar read front'
        [] -> do
           end <- readTVar write
           let front'' = rotate front' end []
           writeTVar read front''
           writeTVar write []
           writeTVar sched front''
      return x

-- | A version of 'readTQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadTQueue :: TQueue a -> STM (Maybe a)
tryReadTQueue c = fmap Just (readTQueue c) `orElse` return Nothing

-- | Efficiently read the entire contents of a 'TQueue' into a list. This
-- function never retries.
--
-- @since 2.4.5
flushTQueue :: TQueue a -> STM [a]
flushTQueue (TQueue read write sched) = do
  xs <- readTVar read
  if null xs
    then
      return []
    else do
      ys <- readTVar write
      writeTVar read []
      unless (null ys) $ writeTVar write []
      writeTVar sched []
      return (xs ++ reverse ys)

-- | Get the next value from the @TQueue@ without removing it,
-- retrying if the channel is empty.
peekTQueue :: TQueue a -> STM a
peekTQueue (TQueue read _write _sched) = do
  front <- readTVar read
  case front of
    [] -> retry
    x:_ -> return x

-- | A version of 'peekTQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryPeekTQueue :: TQueue a -> STM (Maybe a)
tryPeekTQueue (TQueue read _write _sched) = do
  front <- readTVar read
  case front of
    [] -> return Nothing
    x:_ -> return (Just x)

-- |Put a data item back onto a channel, where it will be the next item read.
unGetTQueue :: TQueue a -> a -> STM ()
unGetTQueue (TQueue read _write sched) a = do
  xs <- readTVar read
  sch <- readTVar sched
  writeTVar read (a:xs)
  writeTVar sched (a:sch)

-- |Returns 'True' if the supplied 'TQueue' is empty.
isEmptyTQueue :: TQueue a -> STM Bool
isEmptyTQueue (TQueue read _write _sched) = do
  xs <- readTVar read
  case xs of
    (_:_) -> return False
    [] -> return True
