-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TSem.Strict
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Strict 'TSem': transactional semaphores.
--
-- A 'TSem' holds a counter that is always evaluated to Weak Head Normal
-- Form, so "Control.Concurrent.STM.TSem" is already strict.  This module
-- re-exports it so that every data type in this package has a @.Strict@
-- module.
--
-- @since 2.5.4.0
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TSem.Strict
  ( module Control.Concurrent.STM.TSem
  ) where

import Control.Concurrent.STM.TSem
