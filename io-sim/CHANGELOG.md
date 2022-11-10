# Revsion history of io-sim

## next version

## 0.4.0.0

* support `ghc-9.4` (except on Windows input-output-hk/io-sim#51)
* `MonadCatch` instance for `STM (IOSim s)` monad
* fixed `isEmptyTBQueeuDefault` (thanks to @EMQ-YangM)
* refactored internal implementation of timeouts, changed `SimEventType`
  constructors

## 0.3.0.0

* added `Functor` instance for `SimResult`
* added `MonadFix` instance for `STM (IOSim s)`
* support `ghc-9.2` & `ghc-9.4`

## 0.2.0.0

* First version published on Hackage.
* Depends on `io-classes-0.2.0.0`.

## 0.1.0.0

* Initial version, not released on Hackage.
