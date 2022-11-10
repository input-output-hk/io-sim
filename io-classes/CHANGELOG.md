# Revsion history of io-classes

## next version

## 0.4.0.0

* support `ghc-9.4` (except on Windows input-output-hk/io-sim#51)

## 0.3.0.0

* added missing `MArray` instance for `WrappedSTM`
* added monoid instances for `WrappedSTM`
* added `threadStatus` method of `MonadFork` class
* removed `linktTo` and `linkToOnly`
* added `labelTChan`
* added `flushTQueue`
* split `MonadSTM` into submodules in a similar way that the `stm` package is.
* added `TChan`, `TSem` and `TArray`
* added `interruptible` and `allowInterrupt` to `MonadMaskingState` type class.
* added `unGetTQueue` & `unGetTBQueue`
* support `ghc-9.2` & `ghc-9.4`
* added `MonadMVar`

## 0.2.0.0

* First version published on Hackage.
* added `lengthTBQueue` to `MonadSTM`; `io-classes` now depends on `stm-2.5.0.0`.

## 0.1.0.0

* Initial version, not released on Hackage.
