-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.STM
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Software Transactional Memory: a modular composable concurrency
-- abstraction.  See
--
--  * /Composable memory transactions/, by Tim Harris, Simon Marlow, Simon
--    Peyton Jones, and Maurice Herlihy, in /ACM Conference on Principles
--    and Practice of Parallel Programming/ 2005.
--    <http://research.microsoft.com/Users/simonpj/papers/stm/index.htm>
--
-----------------------------------------------------------------------------

module Control.Monad.STM (
  	STM,
	atomically,
#ifdef __GLASGOW_HASKELL__
	retry,
	orElse,
	check,
#endif
        catchSTM
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Conc
import Control.Monad	( MonadPlus(..) )
#else
import Control.Sequential.STM
#endif

#ifdef __GLASGOW_HASKELL__
instance MonadPlus STM where
  mzero = retry
  mplus = orElse

check :: Bool -> STM a
check b = if b then return undefined else retry
#endif
