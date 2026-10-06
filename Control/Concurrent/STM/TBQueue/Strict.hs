-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TBQueue.Strict
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Strict 'TBQueue': a bounded FIFO queue whose elements are evaluated to
-- Weak Head Normal Form (WHNF) before they are stored.
--
-- This module has the same interface as "Control.Concurrent.STM.TBQueue".
--
-- @since 2.5.4.0
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TBQueue.Strict (
    -- * Strict TBQueue
    TBQueue,
    newTBQueue,
    newTBQueueIO,
    readTBQueue,
    tryReadTBQueue,
    flushTBQueue,
    peekTBQueue,
    tryPeekTBQueue,
    writeTBQueue,
    unGetTBQueue,
    lengthTBQueue,
    isEmptyTBQueue,
    isFullTBQueue,
    capacityTBQueue,
  ) where

import Control.Monad.STM (STM)
import qualified Control.Concurrent.STM.TBQueue as Lazy
import Numeric.Natural (Natural)

-- | A strict (WHNF) variant of the @TBQueue@ from
-- "Control.Concurrent.STM.TBQueue": a bounded FIFO channel whose elements
-- are evaluated to WHNF when they are written.
--
-- @since 2.5.4.0
newtype TBQueue a = TBQueue (Lazy.TBQueue a) deriving Eq

-- | Builds and returns a new instance of 'TBQueue'.
--
-- @since 2.5.4.0
newTBQueue :: Natural   -- ^ maximum number of elements the queue can hold
           -> STM (TBQueue a)
newTBQueue size = fmap TBQueue (Lazy.newTBQueue size)

-- | @IO@ version of 'newTBQueue'.  This is useful for creating top-level
-- 'TBQueue's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'Control.Monad.STM.atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
--
-- @since 2.5.4.0
newTBQueueIO :: Natural -> IO (TBQueue a)
newTBQueueIO size = fmap TBQueue (Lazy.newTBQueueIO size)

-- | Write a value to a 'TBQueue'; blocks if the queue is full.
--
-- Evaluates the value to WHNF before checking whether the queue is full.
--
-- @since 2.5.4.0
writeTBQueue :: TBQueue a -> a -> STM ()
writeTBQueue (TBQueue q) a = Lazy.writeTBQueue q =<< (pure $! a)

-- | Read the next value from the 'TBQueue'.
--
-- @since 2.5.4.0
readTBQueue :: TBQueue a -> STM a
readTBQueue (TBQueue q) = Lazy.readTBQueue q

-- | A version of 'readTBQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
--
-- @since 2.5.4.0
tryReadTBQueue :: TBQueue a -> STM (Maybe a)
tryReadTBQueue (TBQueue q) = Lazy.tryReadTBQueue q

-- | Efficiently read the entire contents of a 'TBQueue' into a list. This
-- function never retries.
--
-- @since 2.5.4.0
flushTBQueue :: TBQueue a -> STM [a]
flushTBQueue (TBQueue q) = Lazy.flushTBQueue q

-- | Get the next value from the @TBQueue@ without removing it,
-- retrying if the channel is empty.
--
-- @since 2.5.4.0
peekTBQueue :: TBQueue a -> STM a
peekTBQueue (TBQueue q) = Lazy.peekTBQueue q

-- | A version of 'peekTBQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
--
-- @since 2.5.4.0
tryPeekTBQueue :: TBQueue a -> STM (Maybe a)
tryPeekTBQueue (TBQueue q) = Lazy.tryPeekTBQueue q

-- | Put a data item back onto a channel, where it will be the next item read.
-- Blocks if the queue is full.
--
-- Evaluates the value to WHNF before checking whether the queue is full.
--
-- @since 2.5.4.0
unGetTBQueue :: TBQueue a -> a -> STM ()
unGetTBQueue (TBQueue q) a = Lazy.unGetTBQueue q =<< (pure $! a)

-- | Return the length of a 'TBQueue'.
--
-- @since 2.5.4.0
lengthTBQueue :: TBQueue a -> STM Natural
lengthTBQueue (TBQueue q) = Lazy.lengthTBQueue q

-- | Returns 'True' if the supplied 'TBQueue' is empty.
--
-- @since 2.5.4.0
isEmptyTBQueue :: TBQueue a -> STM Bool
isEmptyTBQueue (TBQueue q) = Lazy.isEmptyTBQueue q

-- | Returns 'True' if the supplied 'TBQueue' is full.
--
-- @since 2.5.4.0
isFullTBQueue :: TBQueue a -> STM Bool
isFullTBQueue (TBQueue q) = Lazy.isFullTBQueue q

-- | The maximum number of elements the queue can hold.
--
-- @since 2.5.4.0
capacityTBQueue :: TBQueue a -> Natural
capacityTBQueue (TBQueue q) = Lazy.capacityTBQueue q
