{-# LANGUAGE CPP #-}

{- NB: This one fails for GHC 7.6.1 in particular due to GHC#7493.

This was fixed in GHC via
 a006ecdfd381fa75ab16ddb66c3a2b247f359eb8
-}

module Stm065 (main) where

import           Control.Concurrent.STM
import           Control.Monad          (unless)

main :: IO ()
#if defined(GHC_7_6_1)
main = putStrLn "Warning: test disabled for GHC 7.6.1"
#else
main = do
  x <- atomically $ do
         r <- newTVar []
         writeTVar r [2 :: Integer]
         writeTVar r [] `orElse` return ()
         readTVar r

  unless (null x) $ do
    fail (show x)
#endif
