-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TSem
-- Copyright   :  (c) The University of Glasgow 2012
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- 'TSem': transactional semaphores.
--
-- @since 2.4.2
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
module Control.Concurrent.STM.TSem (
      TSem, newTSem, waitTSem, signalTSem
  ) where

import Control.Concurrent.STM
import Control.Monad
import Data.Typeable

-- | 'TSem' is a transactional semaphore.  It holds a certain number
-- of units, and units may be acquired or released by 'waitTSem' and
-- 'signalTSem' respectively.  When the 'TSem' is empty, 'waitTSem'
-- blocks.
--
-- Note that 'TSem' has no concept of fairness, and there is no
-- guarantee that threads blocked in `waitTSem` will be unblocked in
-- the same order; in fact they will all be unblocked at the same time
-- and will fight over the 'TSem'.  Hence 'TSem' is not suitable if
-- you expect there to be a high number of threads contending for the
-- resource.  However, like other STM abstractions, 'TSem' is
-- composable.
--
-- @since 2.4.2
newtype TSem = TSem (TVar Int)
  deriving (Eq, Typeable)

-- | Construct new 'TSem' with an initial counter value.
--
-- A positive initial counter value denotes availability of
-- units 'waitTSem' can acquire.
--
-- The initial counter value can be negative which denotes a resource
-- \"debt\" that requires a respective amount of 'signalTSem'
-- operations to counter-balance.
--
-- @since 2.4.2
newTSem :: Int -> STM TSem
newTSem i = fmap TSem (newTVar i)

-- NOTE: we can't expose a good `TSem -> STM Int' operation as blocked
-- 'waitTSem' aren't reliably reflected in a negative counter value.

-- TODO: Consider adding '{wait,signal}TSemN :: Word -> TSem -> STM ()'
-- variants; NB: 'waitTSemN 0' would *not* be a no-op

-- | Wait on 'TSem' (aka __P__ operation).
--
-- This operation acquires a unit from the semaphore (i.e. decreases
-- the internal counter) and blocks (via 'retry') if no units are
-- available (i.e. if the counter is /not/ positive).
--
-- @since 2.4.2
waitTSem :: TSem -> STM ()
waitTSem (TSem t) = do
  i <- readTVar t
  when (i <= 0) retry
  writeTVar t $! (i-1)


-- TODO: consider using an 'Integer' value for the internal counter
-- value which can never overflow; the computational overhead
-- should hopefully be neglectable in the context of the STM
-- transaction overhead.
--
-- Alternatively, the implementation could block (via 'retry') when
-- the next increment would overflow, i.e. testing for 'maxBound'

-- | Signal a 'TSem' (aka __V__ operation).
--
-- This operation adds\/releases a unit back to the semaphore
-- (i.e. increments the internal counter).
--
-- __NOTE__: The implementation currently does not protect against
-- overflow of the internal 'Int' counter value.
--
-- @since 2.4.2
signalTSem :: TSem -> STM ()
signalTSem (TSem t) = do
  i <- readTVar t
  writeTVar t $! i+1
