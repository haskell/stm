module Async where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

newtype Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
    mvar <- newEmptyMVar
    _ <- forkIO $ do
        x <- action
        putMVar mvar x
    pure (Async mvar)

wait :: Async a -> IO a
wait (Async a) = takeMVar a
