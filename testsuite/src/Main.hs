{-# LANGUAGE CPP #-}

module Main where

import           Test.Framework                 (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit

import qualified Issue9

main :: IO ()
main = do
    putStrLn ("'stm' version under test: " ++ VERSION_stm)
    defaultMain tests
  where
    tests = [
      testGroup "regression"
        [ testCase "issue #9" Issue9.main
        ]
      ]

