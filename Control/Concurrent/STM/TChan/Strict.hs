-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TChan.Strict
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Strict 'TChan': transactional channels whose elements are evaluated to
-- Weak Head Normal Form (WHNF) before they are stored.
--
-- This module has the same interface as "Control.Concurrent.STM.TChan".
--
-- @since 2.5.4.0
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TChan.Strict (
        -- * Strict TChans
        TChan,

        -- ** Construction
        newTChan,
        newTChanIO,
        newBroadcastTChan,
        newBroadcastTChanIO,
        dupTChan,
        cloneTChan,

        -- ** Reading and writing
        readTChan,
        tryReadTChan,
        peekTChan,
        tryPeekTChan,
        writeTChan,
        unGetTChan,
        isEmptyTChan
  ) where

import Control.Monad.STM (STM)
import qualified Control.Concurrent.STM.TChan as Lazy

-- | A strict (WHNF) variant of the @TChan@ from "Control.Concurrent.STM.TChan":
-- an unbounded FIFO channel whose elements are evaluated to WHNF when they
-- are written.
--
-- @since 2.5.4.0
newtype TChan a = TChan (Lazy.TChan a) deriving Eq

-- | Build and return a new instance of 'TChan'
--
-- @since 2.5.4.0
newTChan :: STM (TChan a)
newTChan = fmap TChan Lazy.newTChan

-- | @IO@ version of 'newTChan'.  This is useful for creating top-level
-- 'TChan's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'Control.Monad.STM.atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
--
-- @since 2.5.4.0
newTChanIO :: IO (TChan a)
newTChanIO = fmap TChan Lazy.newTChanIO

-- | Create a write-only 'TChan'.  More precisely, 'readTChan' will
-- throw an exception.  The only way to read a broadcast channel is to
-- duplicate it with 'dupTChan'.
--
-- See 'Control.Concurrent.STM.TChan.newBroadcastTChan' for the rationale.
--
-- @since 2.5.4.0
newBroadcastTChan :: STM (TChan a)
newBroadcastTChan = fmap TChan Lazy.newBroadcastTChan

-- | @IO@ version of 'newBroadcastTChan'.
--
-- @since 2.5.4.0
newBroadcastTChanIO :: IO (TChan a)
newBroadcastTChanIO = fmap TChan Lazy.newBroadcastTChanIO

-- | Write a value to a 'TChan'.
--
-- Evaluates the value to WHNF.
--
-- @since 2.5.4.0
writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan c) a = Lazy.writeTChan c =<< (pure $! a)

-- | Read the next value from the 'TChan'.
--
-- @since 2.5.4.0
readTChan :: TChan a -> STM a
readTChan (TChan c) = Lazy.readTChan c

-- | A version of 'readTChan' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
--
-- @since 2.5.4.0
tryReadTChan :: TChan a -> STM (Maybe a)
tryReadTChan (TChan c) = Lazy.tryReadTChan c

-- | Get the next value from the @TChan@ without removing it,
-- retrying if the channel is empty.
--
-- @since 2.5.4.0
peekTChan :: TChan a -> STM a
peekTChan (TChan c) = Lazy.peekTChan c

-- | A version of 'peekTChan' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
--
-- @since 2.5.4.0
tryPeekTChan :: TChan a -> STM (Maybe a)
tryPeekTChan (TChan c) = Lazy.tryPeekTChan c

-- | Duplicate a 'TChan': the duplicate channel begins empty, but data written to
-- either channel from then on will be available from both.  Hence this creates
-- a kind of broadcast channel, where data written by anyone is seen by
-- everyone else.
--
-- @since 2.5.4.0
dupTChan :: TChan a -> STM (TChan a)
dupTChan (TChan c) = fmap TChan (Lazy.dupTChan c)

-- | Put a data item back onto a channel, where it will be the next item read.
--
-- Evaluates the value to WHNF.
--
-- @since 2.5.4.0
unGetTChan :: TChan a -> a -> STM ()
unGetTChan (TChan c) a = Lazy.unGetTChan c =<< (pure $! a)

-- | Returns 'True' if the supplied 'TChan' is empty.
--
-- @since 2.5.4.0
isEmptyTChan :: TChan a -> STM Bool
isEmptyTChan (TChan c) = Lazy.isEmptyTChan c

-- | Clone a 'TChan': similar to 'dupTChan', but the cloned channel starts with the
-- same content available as the original channel.
--
-- @since 2.5.4.0
cloneTChan :: TChan a -> STM (TChan a)
cloneTChan (TChan c) = fmap TChan (Lazy.cloneTChan c)
