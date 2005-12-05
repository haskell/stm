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
--
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TMVar (
	-- * TVars
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
  ) where

import GHC.Conc

newtype TMVar a = TMVar (TVar (Maybe a))

newTMVar :: a -> STM (TMVar a)
newTMVar a = do
  t <- newTVar (Just a)
  return (TMVar t)

newTMVarIO :: a -> IO (TMVar a)
newTMVarIO a = do
  t <- newTVarIO (Just a)
  return (TMVar t)

newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = do
  t <- newTVar Nothing
  return (TMVar t)

newEmptyTMVarIO :: IO (TMVar a)
newEmptyTMVarIO = do
  t <- newTVarIO Nothing
  return (TMVar t)

takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a  -> do writeTVar t Nothing; return a

tryTakeTMVar :: TMVar a -> STM (Maybe a)
tryTakeTMVar (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> return Nothing
    Just a  -> do writeTVar t Nothing; return (Just a)

putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> do writeTVar t (Just a); return ()
    Just _  -> retry

tryPutTMVar :: TMVar a -> a -> STM Bool
tryPutTMVar (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> do writeTVar t (Just a); return True
    Just _  -> return False

readTMVar :: TMVar a -> STM a
readTMVar (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a  -> return a

swapTMVar :: TMVar a -> a -> STM a
swapTMVar (TMVar t) new = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just old -> do writeTVar t (Just new); return old

isEmptyTMVar :: TMVar a -> STM Bool
isEmptyTMVar (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> return True
    Just _  -> return False
