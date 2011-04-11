{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TMVar
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- TMVar: Transactional MVars, for use in the STM monad
-- (GHC only)
--
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TMVar (
#ifdef __GLASGOW_HASKELL__
	-- * TMVars
	TMVar,
	newTMVar,
	newEmptyTMVar,
	newTMVarIO,
	newEmptyTMVarIO,
	takeTMVar,
	putTMVar,
	readTMVar,	
	swapTMVar,
	tryTakeTMVar,
	tryPutTMVar,
	isEmptyTMVar
#endif
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Conc

import Data.Typeable (Typeable)

newtype TMVar a = TMVar (TVar (Maybe a)) deriving (Eq, Typeable)
{- ^
A 'TMVar' is a synchronising variable, used
for communication between concurrent threads.  It can be thought of
as a box, which may be empty or full.
-}

-- |Create a 'TMVar' which contains the supplied value.
newTMVar :: a -> STM (TMVar a)
newTMVar a = do
  t <- newTVar (Just a)
  return (TMVar t)

-- |@IO@ version of 'newTMVar'.  This is useful for creating top-level
-- 'TMVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTMVarIO :: a -> IO (TMVar a)
newTMVarIO a = do
  t <- newTVarIO (Just a)
  return (TMVar t)

-- |Create a 'TMVar' which is initially empty.
newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = do
  t <- newTVar Nothing
  return (TMVar t)

-- |@IO@ version of 'newEmptyTMVar'.  This is useful for creating top-level
-- 'TMVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newEmptyTMVarIO :: IO (TMVar a)
newEmptyTMVarIO = do
  t <- newTVarIO Nothing
  return (TMVar t)

-- |Return the contents of the 'TMVar'.  If the 'TMVar' is currently
-- empty, the transaction will 'retry'.  After a 'takeTMVar', 
-- the 'TMVar' is left empty.
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
tryTakeTMVar :: TMVar a -> STM (Maybe a)
tryTakeTMVar (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> return Nothing
    Just a  -> do writeTVar t Nothing; return (Just a)

-- |Put a value into a 'TMVar'.  If the 'TMVar' is currently full,
-- 'putTMVar' will 'retry'.
putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> do writeTVar t (Just a); return ()
    Just _  -> retry

-- | A version of 'putTMVar' that does not 'retry'.  The 'tryPutTMVar'
-- function attempts to put the value @a@ into the 'TMVar', returning
-- 'True' if it was successful, or 'False' otherwise.
tryPutTMVar :: TMVar a -> a -> STM Bool
tryPutTMVar (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> do writeTVar t (Just a); return True
    Just _  -> return False

{-|
  This is a combination of 'takeTMVar' and 'putTMVar'; ie. it takes the value
  from the 'TMVar', puts it back, and also returns it.
-}
readTMVar :: TMVar a -> STM a
readTMVar (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a  -> return a

-- |Swap the contents of a 'TMVar' for a new value.
swapTMVar :: TMVar a -> a -> STM a
swapTMVar (TMVar t) new = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just old -> do writeTVar t (Just new); return old

-- |Check whether a given 'TMVar' is empty.
--
-- Notice that the boolean value returned  is just a snapshot of
-- the state of the 'TMVar'. By the time you get to react on its result,
-- the 'TMVar' may have been filled (or emptied) - so be extremely
-- careful when using this operation.   Use 'tryTakeTMVar' instead if possible.
isEmptyTMVar :: TMVar a -> STM Bool
isEmptyTMVar (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> return True
    Just _  -> return False
#endif
