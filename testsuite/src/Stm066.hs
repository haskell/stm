{-# LANGUAGE CPP #-}

{- NB: This one fails for GHC < 7.6 which had a bug exposed via
       nested uses of `orElse` in `stmCommitNestedTransaction`

This was fixed in GHC via
 f184d9caffa09750ef6a374a7987b9213d6db28e
-}

module Stm066 (main) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import           Control.Monad          (unless)

main :: IO ()
main = do
  q <- atomically $ newTQueue
  _ <- forkIO $ atomically $ do
    writeTQueue q (1::Int)
    writeTQueue q 2
    writeTQueue q 3
    writeTQueue q 4
  l <- atomically $ do
         _ <- readTQueueN 1 q 
         readTQueueN 3 q 

  unless (l == [2,3,4]) $
    fail (show l)
