{-# LANGUAGE CPP #-}

module Main where

import           Test.Framework                 (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit

import Control.Concurrent.STM

import qualified Issue9
import qualified Stm052
import qualified Stm064
import qualified Stm065

main :: IO ()
main = do
    putStrLn ("'stm' version under test: " ++ VERSION_stm)
    defaultMain tests
  where
    tests = [
      testGroup "regression"
        [ testCase "issue #9" Issue9.main
        , testCase "stm052" Stm052.main
        , testCase "stm064" Stm064.main
        , testCase "stm065" Stm065.main
        , testCase "issue #76" $ do
            queue <- newTBQueueIO 100
            len <- atomically $ do
              writeTBQueue queue (1 :: Int)
              writeTBQueue queue 2
              _ <- flushTBQueue queue
              lengthTBQueue queue
            len @?= 0
        ]
      ]

