# Change Log

# Circa 2022.09.27 (pre release)

- Module structure of `MonadSTM` changed to follow `stm` package structure.
- default `MonadSTM` api implementations inlined into the type class
- Added `TQueue` and `TBQueue` to `strct-stm` package
- Added `unGetTQueue` and `unGetTBQueue`
- Fixed input-output-hk/ouroboros-network#issues2650
- Added `link2` and `link2Only` and removed `linkTo` and `linkToOnly` to `MonadAsync`
- Added `TChan`, `TSem` and `TArray`
- `MonadTraceSTM`: removed proxy argument from `trace.*IO` functions
- Split `MonadSTM` and `MonadSTM.Strict` into submodules
- Added `flushTQueue` to `MonadSTM`
- Added `cast{TQueue,TBQueue,TChan}` to `strict-stm` package
