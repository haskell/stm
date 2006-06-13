-- Transactional memory for sequential implementations.
-- Transactions do not run concurrently, but are atomic in the face
-- of exceptions.

-- #hide
module Control.Sequential.STM (
	STM, atomically, catchSTM,
	TVar, newTVar, newTVarIO, readTVar, writeTVar
    ) where

import Prelude hiding (catch)
import Control.Exception
import Data.IORef

-- The reference contains a rollback action to be executed on exceptions
newtype STM a = STM (IORef (IO ()) -> IO a)

unSTM :: STM a -> IORef (IO ()) -> IO a
unSTM (STM f) = f

instance Functor STM where
    fmap f (STM m) = STM (fmap f . m)

instance Monad STM where
    return x = STM (const (return x))
    STM m >>= k = STM $ \ r -> do
	x <- m r
	unSTM (k x) r

atomically :: STM a -> IO a
atomically (STM m) = do
    r <- newIORef (return ())
    m r `catch` \ ex -> do
	rollback <- readIORef r
	rollback
	throw ex

catchSTM :: STM a -> (Exception -> STM a) -> STM a
catchSTM (STM m) h = STM $ \ r -> m r `catch` \ ex -> unSTM (h ex) r

newtype TVar a = TVar (IORef a)
    deriving (Eq)

newTVar :: a -> STM (TVar a)
newTVar a = STM (const (newTVarIO a))

newTVarIO :: a -> IO (TVar a)
newTVarIO a = do
    ref <- newIORef a
    return (TVar ref)

readTVar :: TVar a -> STM a
readTVar (TVar ref) = STM (const (readIORef ref))

writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar ref) a = STM $ \ r -> do
    oldval <- readIORef ref
    modifyIORef r (writeIORef ref oldval >>)
    writeIORef ref a
