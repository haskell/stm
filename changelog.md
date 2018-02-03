# Changelog for [`stm` package](http://hackage.haskell.org/package/stm)

## 2.4.5.0 *Feb 2018*

  * Fix space leak in `TBQueue` (gh-2, GHC#14494)

  * Make `signalTSem` resilient against `Int` overflows (gh-4)

  * Make definition of `readTQueue` consistent with `readTBQueue` (gh-3, GHC#9539)

  * Add `flushTQueue` to `Control.Concurrent.STM.TQueue` (gh-1)

  * Add `flushTBQueue` to `Control.Concurrent.STM.TBQueue` (gh-1)

  * Add `signalTSemN` operation (gh-5)


## 2.4.4.1  *Dec 2015*

  * Add support for `base-4.9.0.0`

  * Drop support for GHC 6.12 / `base-4.2`

## 2.4.4  *Dec 2014*

  * Add support for `base-4.8.0.0`

  * Tighten Safe Haskell bounds

  * Add `mkWeakTMVar` to `Control.Concurrent.STM.TMVar`

  * Add `@since`-annotations

## 2.4.3  *Mar 2014*

  * Update behaviour of `newBroadcastTChanIO` to match
    `newBroadcastTChan` in causing an error on a read from the
    broadcast channel

  * Add `mkWeakTVar`

  * Add `isFullTBQueue`

  * Fix `TChan` created via `newBroadcastTChanIO` to throw same
    exception on a `readTChan` as when created via `newBroadcastTChan`

  * Update to Cabal 1.10 format

## 2.4.2  *Nov 2012*

  * Add `Control.Concurrent.STM.TSem` (transactional semaphore)

  * Add Applicative/Alternative instances of STM for GHC <7.0

  * Throw proper exception when `readTChan` called on a broadcast `TChan`

## 2.4  *Jul 2012*

  * Add `Control.Concurrent.STM.TQueue` (a faster `TChan`)

  * Add `Control.Concurrent.STM.TBQueue` (a bounded channel based on `TQueue`)

  * Add `Eq` instance for `TChan`

  * Add `newBroadcastTChan` and `newBroadcastTChanIO`

  * Some performance improvements for `TChan`

  * Add `cloneTChan`
