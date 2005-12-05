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
--
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TChan (
	-- * TChans
	TChan,
	newTChan,
	newTChanIO,
	readTChan,
	writeTChan,
	dupTChan,
	unGetTChan,
	isEmptyTChan
  ) where

import GHC.Conc

-- | 'TChan' is an abstract type representing an unbounded FIFO channel.
data TChan a = TChan (TVar (TVarList a)) (TVar (TVarList a))

type TVarList a = TVar (TList a)
data TList a = TNil | TCons a (TVarList a)

newTChan :: STM (TChan a)
newTChan = do
  hole <- newTVar TNil
  read <- newTVar hole
  write <- newTVar hole
  return (TChan read write)

newTChanIO :: IO (TChan a)
newTChanIO = do
  hole <- newTVarIO TNil
  read <- newTVarIO hole
  write <- newTVarIO hole
  return (TChan read write)

writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan _read write) a = do
  listend <- readTVar write -- listend == TVar pointing to TNil
  new_listend <- newTVar TNil
  writeTVar listend (TCons a new_listend)
  writeTVar write new_listend

readTChan :: TChan a -> STM a
readTChan (TChan read _write) = do
  listhead <- readTVar read
  head <- readTVar listhead
  case head of
    TNil -> retry
    TCons a tail -> do
	writeTVar read tail
	return a

dupTChan :: TChan a -> STM (TChan a)
dupTChan (TChan read write) = do
  hole <- readTVar write  
  new_read <- newTVar hole
  return (TChan new_read write)

unGetTChan :: TChan a -> a -> STM ()
unGetTChan (TChan read _write) a = do
   listhead <- readTVar read
   newhead <- newTVar (TCons a listhead)
   writeTVar read newhead

-- |Returns 'True' if the supplied 'TChan' is empty.
isEmptyTChan :: TChan a -> STM Bool
isEmptyTChan (TChan read write) = do
  listhead <- readTVar read
  head <- readTVar listhead
  case head of
    TNil -> return True
    TCons _ _ -> return False
