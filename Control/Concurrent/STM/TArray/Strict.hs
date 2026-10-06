{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TArray.Strict
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Strict 'TArray': transactional arrays whose elements are evaluated to
-- Weak Head Normal Form (WHNF) before they are stored.
--
-- @since 2.5.4.0
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TArray.Strict (
    TArray
) where

import Control.Monad.STM (STM)
import qualified Control.Concurrent.STM.TArray as Lazy
import Data.Array.Base (MArray(..))

-- | A strict (WHNF) variant of the @TArray@ from
-- "Control.Concurrent.STM.TArray": a transactional array, supporting the
-- 'MArray' interface for mutable arrays, whose elements are evaluated
-- to WHNF when they are written.
--
-- @since 2.5.4.0
newtype TArray i e = TArray (Lazy.TArray i e) deriving Eq

-- | 'newArray' and 'unsafeWrite' evaluate the element to WHNF.
--
-- @since 2.5.4.0
instance MArray TArray e STM where
    getBounds (TArray a) = getBounds a
    getNumElements (TArray a) = getNumElements a
    newArray b e = fmap TArray (newArray b =<< (pure $! e))
    {-# INLINE newArray #-}
    newArray_ b = fmap TArray (newArray_ b)
    unsafeNewArray_ b = fmap TArray (unsafeNewArray_ b)
    unsafeRead (TArray a) i = unsafeRead a i
    unsafeWrite (TArray a) i e = unsafeWrite a i =<< (pure $! e)

-- | 'newArray' and 'unsafeWrite' evaluate the element to WHNF.
-- Writes are slow in @IO@.
--
-- @since 2.5.4.0
instance MArray TArray e IO where
    getBounds (TArray a) = getBounds a
    getNumElements (TArray a) = getNumElements a
    newArray b e = fmap TArray (newArray b =<< (pure $! e))
    {-# INLINE newArray #-}
    newArray_ b = fmap TArray (newArray_ b)
    unsafeNewArray_ b = fmap TArray (unsafeNewArray_ b)
    unsafeRead (TArray a) i = unsafeRead a i
    unsafeWrite (TArray a) i e = unsafeWrite a i =<< (pure $! e)
