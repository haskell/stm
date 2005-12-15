{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TArray
-- Copyright   :  (c) The University of Glasgow 2005
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- TArrays: transactional arrays, for use in the STM monad
--
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TArray (
    TArray
) where

import Control.Monad (replicateM)
import Data.Array (Array)
import Data.Array.Base (listArray, arrEleBottom, unsafeAt, MArray(..), HasBounds(..))
import Data.Ix (rangeSize)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import GHC.Conc (STM)

-- |TArray is a transactional array, supporting the usual 'MArray'
-- interface for mutable arrays.
--
-- It is currently implemented as @Array ix (TVar e)@,
-- but it may be replaced by a more efficient implementation in the future
-- (the interface will remain the same, however).
--
newtype TArray i e = TArray (Array i (TVar e))

instance MArray TArray e STM where
    newArray b e = do
        a <- replicateM (rangeSize b) (newTVar e)
        return $ TArray (listArray b a)
    newArray_ b = do
        a <- replicateM (rangeSize b) (newTVar arrEleBottom)
        return $ TArray (listArray b a)
    unsafeRead (TArray a) i = readTVar $ unsafeAt a i
    unsafeWrite (TArray a) i e = writeTVar (unsafeAt a i) e

instance HasBounds TArray where
    bounds (TArray a) = bounds a

