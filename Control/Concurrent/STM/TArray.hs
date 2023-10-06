{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 904
#define HAS_UNLIFTED_ARRAY 1
#endif

#if defined(HAS_UNLIFTED_ARRAY)
{-# LANGUAGE MagicHash, UnboxedTuples #-}
#endif

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
-- TArrays: transactional arrays, for use in the STM monad.
--
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TArray (
    TArray
) where

import Control.Monad.STM (STM, atomically)
import Data.Typeable (Typeable)
#if defined(HAS_UNLIFTED_ARRAY)
import Control.Concurrent.STM.TVar (readTVar, readTVarIO, writeTVar)
import Data.Array.Base (safeRangeSize, MArray(..))
import Data.Ix (Ix)
import GHC.Conc (STM(..), TVar(..))
import GHC.Exts
import GHC.IO (IO(..))
#else
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Data.Array (Array, bounds, listArray)
import Data.Array.Base (safeRangeSize, unsafeAt, MArray(..), IArray(numElements))
#endif

-- | 'TArray' is a transactional array, supporting the usual 'MArray'
-- interface for mutable arrays.
--
-- It is conceptually implemented as @Array i (TVar e)@.
#if defined(HAS_UNLIFTED_ARRAY)
data TArray i e = TArray
    !i   -- lower bound
    !i   -- upper bound
    !Int -- size
    (Array# (TVar# RealWorld e))
    deriving (Typeable)

instance (Eq i, Eq e) => Eq (TArray i e) where
    (TArray l1 u1 n1 arr1#) == (TArray l2 u2 n2 arr2#) =
        -- each `TArray` has its own `TVar`s, so it's sufficient to compare the first one
        if n1 == 0 then n2 == 0 else l1 == l2 && u1 == u2 && isTrue# (sameTVar# (unsafeFirstT arr1#) (unsafeFirstT arr2#))
      where
        unsafeFirstT :: Array# (TVar# RealWorld e) -> TVar# RealWorld e
        unsafeFirstT arr# = case indexArray# arr# 0# of (# e #) -> e

newTArray# :: Ix i => (i, i) -> e -> State# RealWorld -> (# State# RealWorld, TArray i e #)
newTArray# b@(l, u) e = \s1# ->
    case safeRangeSize b of
        n@(I# n#) -> case newTVar# e s1# of
            (# s2#, initial_tvar# #) -> case newArray# n# initial_tvar# s2# of
                (# s3#, marr# #) ->
                    let go i# = \s4# -> case newTVar# e s4# of
                            (# s5#, tvar# #) -> case writeArray# marr# i# tvar# s5# of
                                s6# -> if isTrue# (i# ==# n# -# 1#) then s6# else go (i# +# 1#) s6#
                    in case unsafeFreezeArray# marr# (if n <= 1 then s3# else go 1# s3#) of
                        (# s7#, arr# #) -> (# s7#, TArray l u n arr# #)

instance MArray TArray e STM where
    getBounds (TArray l u _ _) = return (l, u)
    getNumElements (TArray _ _ n _) = return n
    newArray b e = STM $ newTArray# b e
    unsafeRead (TArray _ _ _ arr#) (I# i#) = case indexArray# arr# i# of
        (# tvar# #) -> readTVar (TVar tvar#)
    unsafeWrite (TArray _ _ _ arr#) (I# i#) e = case indexArray# arr# i# of
        (# tvar# #) -> writeTVar (TVar tvar#) e

-- | Writes are slow in `IO`.
instance MArray TArray e IO where
    getBounds (TArray l u _ _) = return (l, u)
    getNumElements (TArray _ _ n _) = return n
    newArray b e = IO $ newTArray# b e
    unsafeRead (TArray _ _ _ arr#) (I# i#) = case indexArray# arr# i# of
        (# tvar# #) -> readTVarIO (TVar tvar#)
    unsafeWrite (TArray _ _ _ arr#) (I# i#) e = case indexArray# arr# i# of
        (# tvar# #) -> atomically $ writeTVar (TVar tvar#) e
#else
newtype TArray i e = TArray (Array i (TVar e)) deriving (Eq, Typeable)

instance MArray TArray e STM where
    getBounds (TArray a) = return (bounds a)
    getNumElements (TArray a) = return (numElements a)
    newArray b e = do
        a <- rep (safeRangeSize b) (newTVar e)
        return $ TArray (listArray b a)
    unsafeRead (TArray a) i = readTVar $ unsafeAt a i
    unsafeWrite (TArray a) i e = writeTVar (unsafeAt a i) e

    {-# INLINE newArray #-}

-- | Writes are slow in `IO`.
instance MArray TArray e IO where
    getBounds (TArray a) = return (bounds a)
    getNumElements (TArray a) = return (numElements a)
    newArray b e = do
        a <- rep (safeRangeSize b) (newTVarIO e)
        return $ TArray (listArray b a)
    unsafeRead (TArray a) i = readTVarIO $ unsafeAt a i
    unsafeWrite (TArray a) i e = atomically $ writeTVar (unsafeAt a i) e

    {-# INLINE newArray #-}

-- | Like 'replicateM', but uses an accumulator to prevent stack overflows.
-- Unlike 'replicateM', the returned list is in reversed order.
-- This doesn't matter though since this function is only used to create
-- arrays with identical elements.
rep :: Monad m => Int -> m a -> m [a]
rep n m = go n []
    where
      go 0 xs = return xs
      go i xs = do
          x <- m
          go (i - 1) (x : xs)
#endif
