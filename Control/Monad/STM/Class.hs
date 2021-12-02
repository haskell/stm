{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.STM
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Class of monads based on @STM@.
-----------------------------------------------------------------------------

module Control.Monad.STM.Class (
        MonadSTM(..)
  ) where

import GHC.Conc (STM)


-- | Monads in which 'STM' computations may be embedded.
-- Any monad built by applying a sequence of monad transformers to the
-- 'STM' monad will be an instance of this class.
--
-- Instances should satisfy the following laws, which state that 'liftSTM'
-- is a transformer of monads:
--
-- * @'liftSTM' . 'return' = 'return'@
--
-- * @'liftSTM' (m >>= f) = 'liftSTM' m >>= ('liftSTM' . f)@
class Monad m => MonadSTM m where
    -- | Lift a computation from the 'STM' monad.
    liftSTM :: STM a -> m a

-- | @since FIXME
instance MonadSTM STM where
    liftSTM = id
