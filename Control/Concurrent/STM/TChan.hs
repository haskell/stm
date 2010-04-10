{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TChan
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- TChan: Transactional channels
-- (GHC only)
--
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TChan (
#ifdef __GLASGOW_HASKELL__
	-- * TChans
	TChan,
	newTChan,
	newTChanIO,
	readTChan,
	writeTChan,
	dupTChan,
	unGetTChan,
	isEmptyTChan
#endif
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Conc

import Data.Typeable (Typeable)

-- | 'TChan' is an abstract type representing an unbounded FIFO channel.
data TChan a = TChan (TVar (TVarList a)) (TVar (TVarList a)) deriving Typeable

type TVarList a = TVar (TList a)
data TList a = TNil | TCons a (TVarList a)

-- |Build and returns a new instance of 'TChan'
newTChan :: STM (TChan a)
newTChan = do
  hole <- newTVar TNil
  read <- newTVar hole
  write <- newTVar hole
  return (TChan read write)

-- |@IO@ version of 'newTChan'.  This is useful for creating top-level
-- 'TChan's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTChanIO :: IO (TChan a)
newTChanIO = do
  hole <- newTVarIO TNil
  read <- newTVarIO hole
  write <- newTVarIO hole
  return (TChan read write)

-- |Write a value to a 'TChan'.
writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan _read write) a = do
  listend <- readTVar write -- listend == TVar pointing to TNil
  new_listend <- newTVar TNil
  writeTVar listend (TCons a new_listend)
  writeTVar write new_listend

-- |Read the next value from the 'TChan'.
readTChan :: TChan a -> STM a
readTChan (TChan read _write) = do
  listhead <- readTVar read
  head <- readTVar listhead
  case head of
    TNil -> retry
    TCons a tail -> do
	writeTVar read tail
	return a

-- |Duplicate a 'TChan': the duplicate channel begins empty, but data written to
-- either channel from then on will be available from both.  Hence this creates
-- a kind of broadcast channel, where data written by anyone is seen by
-- everyone else.
dupTChan :: TChan a -> STM (TChan a)
dupTChan (TChan _read write) = do
  hole <- readTVar write  
  new_read <- newTVar hole
  return (TChan new_read write)

-- |Put a data item back onto a channel, where it will be the next item read.
unGetTChan :: TChan a -> a -> STM ()
unGetTChan (TChan read _write) a = do
   listhead <- readTVar read
   newhead <- newTVar (TCons a listhead)
   writeTVar read newhead

-- |Returns 'True' if the supplied 'TChan' is empty.
isEmptyTChan :: TChan a -> STM Bool
isEmptyTChan (TChan read _write) = do
  listhead <- readTVar read
  head <- readTVar listhead
  case head of
    TNil -> return True
    TCons _ _ -> return False
#endif
