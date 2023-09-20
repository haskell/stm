{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, RankNTypes, TypeApplications #-}

import Control.Concurrent.Async
import Control.Monad
import Data.Foldable (traverse_)
import System.Environment
import Test.Tasty (localOption)
import Test.Tasty.Bench

import Control.Concurrent.Chan as Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TBQueue

class Channel c where
    newc :: IO (c a)
    readc :: c a -> IO a
    writec :: c a -> a -> IO ()

instance Channel Chan where
    newc = newChan
    readc = Chan.readChan
    writec = Chan.writeChan

instance Channel TChan where
    newc = newTChanIO
    readc c = atomically $ readTChan c
    writec c x = atomically $ writeTChan c x

instance Channel TQueue where
    newc = newTQueueIO
    readc c = atomically $ readTQueue c
    writec c x = atomically $ writeTQueue c x

instance Channel TBQueue where
    newc = newTBQueueIO 4096
    readc c = atomically $ readTBQueue c
    writec c x = atomically $ writeTBQueue c x

-- concurrent writing and reading with single producer, single consumer
concurrentSpsc :: forall c. (Channel c) => Int -> IO ()
concurrentSpsc n = do
    c :: c Int <- newc
    writer <- async $ replicateM_ n $ writec c 1
    reader <- async $ replicateM_ n $ readc c
    wait writer
    wait reader

-- concurrent writing and reading with multiple producers, multiple consumers
concurrentMpmc :: forall c. (Channel c) => Int -> IO ()
concurrentMpmc n = do
    c :: c Int <- newc
    writers <- replicateM 10 $ async $ replicateM_ (n `div` 10) $ writec c 1
    readers <- replicateM 10 $ async $ replicateM_ (n `div` 10) $ readc c
    traverse_ wait writers
    traverse_ wait readers

-- bulk write, then bulk read
bulk :: forall c. (Channel c) => Int -> IO ()
bulk n = do
    c :: c Int <- newc
    replicateM_ n $ writec c 1
    replicateM_ n $ readc c

-- bursts of bulk writes, then bulk reads
burst :: forall c. (Channel c) => Int -> Int -> IO ()
burst k n = do
    c :: c Int <- newc
    replicateM_ k $ do
        replicateM_ (n `div` k) $ writec c 1
        replicateM_ (n `div` k) $ readc c

main :: IO ()
main = defaultMain
    [ localOption WallTime $ bgroup "concurrent spsc"
        [ bench "Chan" $ whnfAppIO (concurrentSpsc @Chan) n
        , bench "TChan" $ whnfAppIO (concurrentSpsc @TChan) n
        , bench "TQueue" $ whnfAppIO (concurrentSpsc @TQueue) n
        , bench "TBQueue" $ whnfAppIO (concurrentSpsc @TBQueue) n
        ]
    , localOption WallTime $ bgroup "concurrent mpmc"
        [ bench "Chan" $ whnfAppIO (concurrentMpmc @Chan) n
        , bench "TChan" $ whnfAppIO (concurrentMpmc @TChan) n
        , bench "TQueue" $ whnfAppIO (concurrentMpmc @TQueue) n
        , bench "TBQueue" $ whnfAppIO (concurrentMpmc @TBQueue) n
        ]
    , bgroup "bulk"
        [ bench "Chan" $ whnfAppIO (bulk @Chan) n
        , bench "TChan" $ whnfAppIO (bulk @TChan) n
        , bench "TQueue" $ whnfAppIO (bulk @TQueue) n
        ]
    , bgroup "burst"
        [ bench "Chan" $ whnfAppIO (burst @Chan 1000) n
        , bench "TChan" $ whnfAppIO (burst @TChan 1000) n
        , bench "TQueue" $ whnfAppIO (burst @TQueue 1000) n
        , bench "TBQueue" $ whnfAppIO (burst @TBQueue 1000) n
        ]
    ]
  where
    n = 2000000
