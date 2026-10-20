{-# LANGUAGE MagicHash, UnboxedTuples #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TMVar.Strict
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Strict 'TMVar': transactional MVars whose content is evaluated to
-- Weak Head Normal Form (WHNF) before it is stored.
--
-- This module has the same interface as "Control.Concurrent.STM.TMVar".
--
-- @since 2.5.4.0
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TMVar.Strict (
        -- * Strict TMVars
        TMVar,
        newTMVar,
        newEmptyTMVar,
        newTMVarIO,
        newEmptyTMVarIO,
        takeTMVar,
        putTMVar,
        readTMVar,
        writeTMVar,
        tryReadTMVar,
        tryReadTMVarIO,
        swapTMVar,
        tryTakeTMVar,
        tryPutTMVar,
        isEmptyTMVar,
        isEmptyTMVarIO,
        mkWeakTMVar
  ) where

import GHC.Base
import GHC.Conc
import GHC.Weak

-- | A strict (WHNF) variant of the @TMVar@ from "Control.Concurrent.STM.TMVar".
--
-- A 'TMVar' is a synchronising variable, used for communication between
-- concurrent threads.  It can be thought of as a box, which may be empty or
-- full.  Every operation that puts a value in the box first evaluates that
-- value to WHNF.
--
-- @since 2.5.4.0
newtype TMVar a = TMVar (TVar (Maybe a)) deriving Eq

-- | Create a 'TMVar' which contains the supplied value.
--
-- Evaluates the value to WHNF.
--
-- @since 2.5.4.0
newTMVar :: a -> STM (TMVar a)
newTMVar a = do
  a' <- pure $! a
  t <- newTVar (Just a')
  return (TMVar t)

-- | @IO@ version of 'newTMVar'.  This is useful for creating top-level
-- 'TMVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
--
-- Evaluates the value to WHNF.
--
-- @since 2.5.4.0
newTMVarIO :: a -> IO (TMVar a)
newTMVarIO a = do
  a' <- pure $! a
  t <- newTVarIO (Just a')
  return (TMVar t)

-- | Create a 'TMVar' which is initially empty.
--
-- @since 2.5.4.0
newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = do
  t <- newTVar Nothing
  return (TMVar t)

-- | @IO@ version of 'newEmptyTMVar'.  This is useful for creating top-level
-- 'TMVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
--
-- @since 2.5.4.0
newEmptyTMVarIO :: IO (TMVar a)
newEmptyTMVarIO = do
  t <- newTVarIO Nothing
  return (TMVar t)

-- | Return the contents of the 'TMVar'.  If the 'TMVar' is currently
-- empty, the transaction will 'retry'.  After a 'takeTMVar',
-- the 'TMVar' is left empty.
--
-- @since 2.5.4.0
takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a  -> do writeTVar t Nothing; return a

-- | A version of 'takeTMVar' that does not 'retry'.  The 'tryTakeTMVar'
-- function returns 'Nothing' if the 'TMVar' was empty, or @'Just' a@ if
-- the 'TMVar' was full with contents @a@.  After 'tryTakeTMVar', the
-- 'TMVar' is left empty.
--
-- @since 2.5.4.0
tryTakeTMVar :: TMVar a -> STM (Maybe a)
tryTakeTMVar (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> return Nothing
    Just a  -> do writeTVar t Nothing; return (Just a)

-- | Put a value into a 'TMVar'.  If the 'TMVar' is currently full,
-- 'putTMVar' will 'retry'.
--
-- Evaluates the value to WHNF before checking whether the 'TMVar' is full.
--
-- @since 2.5.4.0
putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar t) a = do
  a' <- pure $! a
  m <- readTVar t
  case m of
    Nothing -> do writeTVar t (Just a'); return ()
    Just _  -> retry

-- | A version of 'putTMVar' that does not 'retry'.  The 'tryPutTMVar'
-- function attempts to put the value @a@ into the 'TMVar', returning
-- 'True' if it was successful, or 'False' otherwise.
--
-- Evaluates the value to WHNF before checking whether the 'TMVar' is full.
--
-- @since 2.5.4.0
tryPutTMVar :: TMVar a -> a -> STM Bool
tryPutTMVar (TMVar t) a = do
  a' <- pure $! a
  m <- readTVar t
  case m of
    Nothing -> do writeTVar t (Just a'); return True
    Just _  -> return False

-- | This is a combination of 'takeTMVar' and 'putTMVar'; ie. it
-- takes the value from the 'TMVar', puts it back, and also returns
-- it.
--
-- @since 2.5.4.0
readTMVar :: TMVar a -> STM a
readTMVar (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a  -> return a

-- | A version of 'readTMVar' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
--
-- @since 2.5.4.0
tryReadTMVar :: TMVar a -> STM (Maybe a)
tryReadTMVar (TMVar t) = readTVar t

-- | A version of 'readTMVar' which does not retry. This is
-- equivalent to
--
-- >  tryReadTMVarIO = atomically . tryReadTMVar
--
-- but works much faster, because it doesn't perform a complete
-- transaction, it just attempts to read the current value of
-- the 'TMVar'.
--
-- @since 2.5.4.0
tryReadTMVarIO :: TMVar a -> IO (Maybe a)
tryReadTMVarIO (TMVar t) = readTVarIO t

-- | Swap the contents of a 'TMVar' for a new value.
--
-- Evaluates the new value to WHNF before checking whether the 'TMVar' is
-- empty.
--
-- @since 2.5.4.0
swapTMVar :: TMVar a -> a -> STM a
swapTMVar (TMVar t) new = do
  new' <- pure $! new
  m <- readTVar t
  case m of
    Nothing -> retry
    Just old -> do writeTVar t (Just new'); return old

-- | Non-blocking write of a new value to a 'TMVar'.
-- Puts if empty. Replaces if populated.
--
-- Evaluates the new value to WHNF.
--
-- @since 2.5.4.0
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar (TMVar t) new = writeTVar t . Just =<< (pure $! new)

-- | Check whether a given 'TMVar' is empty.
--
-- @since 2.5.4.0
isEmptyTMVar :: TMVar a -> STM Bool
isEmptyTMVar (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> return True
    Just _  -> return False

-- | @IO@ version of 'isEmptyTMVar'.
--
-- @since 2.5.4.0
isEmptyTMVarIO :: TMVar a -> IO Bool
isEmptyTMVarIO (TMVar t) = do
  m <- readTVarIO t
  case m of
    Nothing -> return True
    Just _  -> return False

-- | Make a 'Weak' pointer to a 'TMVar', using the second argument as
-- a finalizer to run when the 'TMVar' is garbage-collected.
--
-- @since 2.5.4.0
mkWeakTMVar :: TMVar a -> IO () -> IO (Weak (TMVar a))
mkWeakTMVar tmv@(TMVar (TVar t#)) (IO finalizer) = IO $ \s ->
    case mkWeak# t# tmv finalizer s of (# s1, w #) -> (# s1, Weak w #)
