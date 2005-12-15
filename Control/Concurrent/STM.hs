-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Software Transactional Memory: a modular composable concurrency
-- abstraction.
--
-----------------------------------------------------------------------------

module Control.Concurrent.STM (
	-- * The STM monad and basic operations
  	STM,
	atomically,
	retry,
	orElse,
	check,
        catchSTM,
	registerDelay,

	-- * TVars
	module Control.Concurrent.STM.TVar,

	-- * TMVars
	module Control.Concurrent.STM.TMVar,

	-- * TChan
	module Control.Concurrent.STM.TChan,

	-- * TArray
	module Control.Concurrent.STM.TArray
  ) where

import GHC.Conc
import Control.Monad	( MonadPlus(..) )
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TArray

instance MonadPlus STM where
  mzero = retry
  mplus = orElse

check :: Bool -> STM a
check b = if b then return undefined else retry
