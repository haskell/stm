{-# LANGUAGE CPP #-}

-- See https://github.com/haskell/stm/pull/76.
--
-- Test-case contributed by Nikita Volkov <nikita.y.volkov@mail.ru>.
--
-- This bug is observable in version `stm-2.5.2.1`.

module Issue76 (main) where

import Control.Concurrent.STM
import Data.Foldable
import Test.HUnit

main :: IO ()
#if MIN_VERSION_stm(2,4,5)
main = do
  queue <- newTBQueueIO 100 :: IO (TBQueue Int)
  lengthAfterFlush <- atomically $ do
    writeTBQueue queue 1
    writeTBQueue queue 2
    _ <- flushTBQueue queue
    lengthTBQueue queue
  assertEqual "" 0 lengthAfterFlush
#else
-- test-case not applicable; `flushTBQueue` was only added in 2.4.5.0
main = return ()
#endif
