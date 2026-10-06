-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.Strict
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Software Transactional Memory with strict (WHNF) data types.
--
-- This module is the counterpart of "Control.Concurrent.STM": it exports
-- the 'STM' monad together with the strict variants of the transactional
-- data types.  Every operation that stores a value in one of these data
-- types first evaluates that value to Weak Head Normal Form, which prevents
-- the accumulation of thunks inside the data types (a common source of
-- space leaks).
--
-- @since 2.5.4.0
-----------------------------------------------------------------------------

module Control.Concurrent.STM.Strict (
        module Control.Monad.STM,
        module Control.Concurrent.STM.TVar.Strict,
        module Control.Concurrent.STM.TMVar.Strict,
        module Control.Concurrent.STM.TChan.Strict,
        module Control.Concurrent.STM.TQueue.Strict,
        module Control.Concurrent.STM.TBQueue.Strict,
        module Control.Concurrent.STM.TArray.Strict
  ) where

import Control.Monad.STM
import Control.Concurrent.STM.TVar.Strict
import Control.Concurrent.STM.TMVar.Strict
import Control.Concurrent.STM.TChan.Strict
import Control.Concurrent.STM.TQueue.Strict
import Control.Concurrent.STM.TBQueue.Strict
import Control.Concurrent.STM.TArray.Strict
