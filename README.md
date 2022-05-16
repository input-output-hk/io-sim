# io-sim

`IOSim` is an simulator monad which supports:

* asynchronous exceptions
* simulated time
* timeout API
* software transaction memory (STM)
* concurrency: both low level `forkIO` as well as `async` style
* strict STM
* access to lazy ST
* schedule discovery (see [IOSimPOR](./how-to-use-IOSimPOR.md))
* eventlog
* dynamic tracing
* tracing committed changes to `TVar`, `TMVar`s, ...
* labeling of threads, `TVar`'s, ...

`io-classes` provide a common interface, which allow to write code which can be
run in both real `IO` and `IOSim`. It is a drop-in replacement for `IO`.

One of the principles of `io-classes` was to stay as close to `IO` as possible,
thus most of the `IO` instances are directly referring to `base`, `async` api.
However we made some differences, which are reported below.

`io-classes` supports a novel hierarchy for error handling monads as well more
familiar `exception` style. The new hierarchy provides `bracket` and
`finally` functions in the `MonadThrow` class, while `catch` style operators
are provided by a super-class `MonadCatch`. Both `bracket` and `finally` are
the most common interface used to write code with robust exception handling,
exposing them through the more basic `MonadThrow` class informs the reader
/ reviewer that no tricky error handling is done in that section of the code
base.

`IOSim` exposes a detailed trace, which can be enhanced by labelling threads,
or mutable variables, tracing `Dynamic` values (which can be recovered from the
trace) or simple `String` based tracing. It has been used to develop & test
a complex concurrent system ([ouroboros-network][ouroboros-network]), in
particular

* write network simulations, to verify a complex networking stack;
* write disk IO simulations, to verify database solution.

## Packages

* `io-sim`: provides two simulator monads: `IOSim` and `IOSimPOR` - an enhanced
  `IOSim` version with schedule discovery capabilities.
* `io-classes`: class bases interface
* `strict-stm`: strict STM operations

[ouroboros-network]: https://github.com/input-output-hk/ouroboros-network


## Differences from `base`, `async` or `exceptions` packages

* `threadDelay` is using `DiffTime` (which is measured in _seconds_ rather than _microseconds_).
* `regiterDelay` is using `DiffTime`
* `timeout` is using `DiffTime`
* `getMonotonicTime` returns `Time` (a newtype wrapper around `DiffTime`)

### Minor differences

Some of the types have more general type signatures, e.g.

```
type Async :: (Type -> Type) -> Type -> Type
```

The first type of kind `Type -> Type` describe the monad which could be
instantiated to `IO`, `IOSim` or some other monad stack build with monad
transformers. The same applies to many other types, e.g. `TVar`, `TMVar`.

The types although similar to the original
are not the same as the ones that come from `base`, `async`, or
`excpetions` packages:

* `Handler` (origin: `base`)
* `MaskingState` (origin: `base`)
* `Concurrently` (origin: `async`)
* `ExceptionInLinkedThread` (origin: `async`)
* `ExitCase` (origin: `exceptions`)


### Issues

Although new issues should be reported in this repository, we still have a list
of issues opened in the `ouroboros-network` repository:

* [io-sim issues][io-sim-issues]
* [io-classes issues][io-sim-issues]

[ouroboros-network]: https://github.com/input-output-hk/ouroboros-network
[io-sim-issues]: https://github.com/input-output-hk/ouroboros-network/issues?q=is%3Aopen+is%3Aissue+label%3Aio-sim
[io-classes-issues]: https://github.com/input-output-hk/ouroboros-network/issues?q=is%3Aopen+is%3Aissue+label%3Aio-classes
