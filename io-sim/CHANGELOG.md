# Revsion history of io-sim

## 1.2.0.0

### Breaking changes

* `selectTraceEvents`, `selectTraceEvents'` catpure time of events.
* Added select function which capture the time of the trace events:
  - `selectTraceEventsDynamicWithTime`
  - `selectTraceEventsDynamicWithTime'`
  - `selectTraceEventsSayWithTime`
  - `selectTraceEventsSayWithTime'`

### Non breaking changes

* Provide `MonadInspectMVar` instance for `IOSim`.
- Added NFData & NoThunks instances for `ThreadId`

## 1.1.0.0

### Non breaking changes

* `io-classes-1.1.0.0`

## 1.0.0.1

### Non breaking changes

* Support `ghc-9.6`.

## 1.0.0.0

### Breaking changes

* Support refactored `MonadTimer`, and new `MonadTimerFancy`, `MonadTimeNSec`
  monad classes.

## 0.6.0.0

### Breaking changes

* Added `TimeoutId` to `EventThreadDelay` and `EventThreadFired` events.

### Non breaking changes

* Fixed `threadDelay` in presence of asynchronous exceptions (in `IOSim` and `IOSimPOR`) (#80).
* Fixed bug in `IOSim` & `IOSimPOR` which resulted in reusing existing
  `ThreadId`s (#80).

## 0.5.0.0

* `MVar` type family is not injective anymore.
* Removed default implementation of `readMVar` in the `MonadMVar` type class.
* Moved `MVarDefault` to `io-sim` package.

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
