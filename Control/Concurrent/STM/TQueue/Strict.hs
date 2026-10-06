-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TQueue.Strict
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Strict 'TQueue': an unbounded FIFO queue whose elements are evaluated to
-- Weak Head Normal Form (WHNF) before they are stored.
--
-- This module has the same interface as "Control.Concurrent.STM.TQueue".
--
-- @since 2.5.4.0
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TQueue.Strict (
        -- * Strict TQueue
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

import Control.Monad.STM (STM)
import qualified Control.Concurrent.STM.TQueue as Lazy

-- | A strict (WHNF) variant of the @TQueue@ from "Control.Concurrent.STM.TQueue":
-- an unbounded FIFO channel whose elements are evaluated to WHNF when they
-- are written.
--
-- @since 2.5.4.0
newtype TQueue a = TQueue (Lazy.TQueue a) deriving Eq

-- | Build and returns a new instance of 'TQueue'
--
-- @since 2.5.4.0
newTQueue :: STM (TQueue a)
newTQueue = fmap TQueue Lazy.newTQueue

-- | @IO@ version of 'newTQueue'.  This is useful for creating top-level
-- 'TQueue's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'Control.Monad.STM.atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
--
-- @since 2.5.4.0
newTQueueIO :: IO (TQueue a)
newTQueueIO = fmap TQueue Lazy.newTQueueIO

-- | Write a value to a 'TQueue'.
--
-- Evaluates the value to WHNF.
--
-- @since 2.5.4.0
writeTQueue :: TQueue a -> a -> STM ()
writeTQueue (TQueue q) a = Lazy.writeTQueue q =<< (pure $! a)

-- | Read the next value from the 'TQueue'.
--
-- @since 2.5.4.0
readTQueue :: TQueue a -> STM a
readTQueue (TQueue q) = Lazy.readTQueue q

-- | A version of 'readTQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
--
-- @since 2.5.4.0
tryReadTQueue :: TQueue a -> STM (Maybe a)
tryReadTQueue (TQueue q) = Lazy.tryReadTQueue q

-- | Efficiently read the entire contents of a 'TQueue' into a list. This
-- function never retries.
--
-- @since 2.5.4.0
flushTQueue :: TQueue a -> STM [a]
flushTQueue (TQueue q) = Lazy.flushTQueue q

-- | Get the next value from the @TQueue@ without removing it,
-- retrying if the channel is empty.
--
-- @since 2.5.4.0
peekTQueue :: TQueue a -> STM a
peekTQueue (TQueue q) = Lazy.peekTQueue q

-- | A version of 'peekTQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
--
-- @since 2.5.4.0
tryPeekTQueue :: TQueue a -> STM (Maybe a)
tryPeekTQueue (TQueue q) = Lazy.tryPeekTQueue q

-- | Put a data item back onto a channel, where it will be the next item read.
--
-- Evaluates the value to WHNF.
--
-- @since 2.5.4.0
unGetTQueue :: TQueue a -> a -> STM ()
unGetTQueue (TQueue q) a = Lazy.unGetTQueue q =<< (pure $! a)

-- | Returns 'True' if the supplied 'TQueue' is empty.
--
-- @since 2.5.4.0
isEmptyTQueue :: TQueue a -> STM Bool
isEmptyTQueue (TQueue q) = Lazy.isEmptyTQueue q
