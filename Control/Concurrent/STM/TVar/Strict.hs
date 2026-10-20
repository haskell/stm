{-# LANGUAGE MagicHash, UnboxedTuples #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TVar.Strict
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Strict 'TVar': transactional variables whose content is evaluated to
-- Weak Head Normal Form (WHNF) before it is stored.
--
-- This module has the same interface as "Control.Concurrent.STM.TVar",
-- with one exception: there is no @modifyTVar'@, because 'modifyTVar' is
-- already strict.
--
-- @since 2.5.4.0
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TVar.Strict (
        -- * Strict TVars
        TVar,
        newTVar,
        newTVarIO,
        readTVar,
        readTVarIO,
        writeTVar,
        modifyTVar,
        stateTVar,
        swapTVar,
        registerDelay,
        mkWeakTVar
  ) where

import Control.Monad.STM (STM)
import qualified Control.Concurrent.STM.TVar as Lazy
import qualified GHC.Conc as GHC (TVar(..))
import GHC.Exts (mkWeak#)
import GHC.IO (IO(..))
import GHC.Weak (Weak(..))

-- | A strict (WHNF) variant of the @TVar@ from "Control.Concurrent.STM.TVar".
--
-- Every operation that stores a value in the variable first evaluates that
-- value to WHNF.
--
-- @since 2.5.4.0
newtype TVar a = TVar (Lazy.TVar a) deriving Eq

-- | Create a new 'TVar' holding a value supplied.
--
-- Evaluates the value to WHNF.
--
-- @since 2.5.4.0
newTVar :: a -> STM (TVar a)
newTVar a = fmap TVar (Lazy.newTVar =<< (pure $! a))

-- | @IO@ version of 'newTVar'.  This is useful for creating top-level
-- 'TVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'Control.Monad.STM.atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
--
-- Evaluates the value to WHNF.
--
-- @since 2.5.4.0
newTVarIO :: a -> IO (TVar a)
newTVarIO a = fmap TVar (Lazy.newTVarIO =<< (pure $! a))

-- | Return the current value stored in a 'TVar'.
--
-- @since 2.5.4.0
readTVar :: TVar a -> STM a
readTVar (TVar t) = Lazy.readTVar t

-- | Return the current value stored in a 'TVar'.
-- This is equivalent to
--
-- >  readTVarIO = atomically . readTVar
--
-- but works much faster, because it doesn't perform a complete
-- transaction, it just reads the current value of the 'TVar'.
--
-- @since 2.5.4.0
readTVarIO :: TVar a -> IO a
readTVarIO (TVar t) = Lazy.readTVarIO t

-- | Write the supplied value into a 'TVar'.
--
-- Evaluates the value to WHNF.
--
-- @since 2.5.4.0
writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar t) a = Lazy.writeTVar t =<< (pure $! a)

-- | Mutate the contents of a 'TVar'.
--
-- Evaluates the new value to WHNF.
--
-- @since 2.5.4.0
modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar (TVar t) f = Lazy.modifyTVar' t f
{-# INLINE modifyTVar #-}

-- | Like 'modifyTVar' but the function is a simple state transition that can
-- return a side value which is passed on as the result of the 'STM'.
--
-- Evaluates the new state to WHNF. The side value is not evaluated.
--
-- @since 2.5.4.0
stateTVar :: TVar s -> (s -> (a, s)) -> STM a
stateTVar (TVar t) f = do
   s <- Lazy.readTVar t
   case f s of
     (a, s') -> do
       Lazy.writeTVar t $! s'
       return a
{-# INLINE stateTVar #-}

-- | Swap the contents of a 'TVar' for a new value.
--
-- Evaluates the new value to WHNF.
--
-- @since 2.5.4.0
swapTVar :: TVar a -> a -> STM a
swapTVar (TVar t) new = Lazy.swapTVar t =<< (pure $! new)
{-# INLINE swapTVar #-}

-- | Switch the value of returned 'TVar' from initial value 'False' to 'True'
-- after a given number of microseconds. The caveats associated with
-- 'Control.Concurrent.threadDelay' also apply.
--
-- @since 2.5.4.0
registerDelay :: Int -> IO (TVar Bool)
registerDelay = fmap TVar . Lazy.registerDelay

-- | Make a 'Weak' pointer to a 'TVar', using the second argument as
-- a finalizer to run when 'TVar' is garbage-collected
--
-- @since 2.5.4.0
mkWeakTVar :: TVar a -> IO () -> IO (Weak (TVar a))
mkWeakTVar t@(TVar (GHC.TVar t#)) (IO finalizer) = IO $ \s ->
    case mkWeak# t# t finalizer s of (# s1, w #) -> (# s1, Weak w #)
