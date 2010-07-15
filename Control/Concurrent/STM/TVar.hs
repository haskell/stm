-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TVar
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- TVar: Transactional variables
--
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TVar (
	-- * TVars
	TVar,
	newTVar,
	readTVar,
	writeTVar,
	newTVarIO,
        readTVarIO,
#ifdef __GLASGOW_HASKELL__
	registerDelay
#endif
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Conc
#else
import Control.Sequential.STM
#endif

#if ! (MIN_VERSION_base(4,2,0))
readTVarIO = atomically . readTVar
#endif
