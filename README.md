[![Haskell CI](https://img.shields.io/github/workflow/status/input-output-hk/io-sim/Haskell%20CI?label=Build&style=for-the-badge)](https://github.com/input-output-hk/io-sim/actions/workflows/haskell.yml)
[![handbook](https://img.shields.io/badge/policy-Cardano%20Engineering%20Handbook-informational?style=for-the-badge)](https://input-output-hk.github.io/cardano-engineering-handbook)

# io-sim


`IOSim` is an simulator monad which supports:

* asynchronous exceptions
* simulated time
* timeout API
* software transaction memory (STM)
* concurrency: both low level `forkIO` as well as `async` style
* strict STM
* access to lazy ST
* schedule discovery (see [IOSimPOR][io-sim-por-how-to])
* eventlog
* dynamic tracing
* tracing committed changes to `TVar`, `TMVar`s, etc.
* labelling of threads, `TVar`'s, etc.

`io-classes` provides an interface, which allows to write code which can be run
in both real `IO` and `IOSim`.  It is a drop-in replacement for `IO`, and
supports interfaces commonly known from `base`, `exceptions`, `stm`, `async` or
`time` packages.

One of the principles of `io-classes` was to stay as close to `IO` as possible,
thus most of the `IO` instances are directly referring to `base` or `async` api.
However we made some differences, which are reported below.

`io-classes` supports a novel hierarchy for error handling monads as well more
familiar `exception` style.  The new hierarchy provides `bracket` and
`finally` functions in the `MonadThrow` class, while `catch` style operators
are provided by a super-class `MonadCatch`.  Both `bracket` and `finally` are
the most common functions used to write code with robust exception handling,
exposing them through the more basic `MonadThrow` class informs the reader
/ reviewer that no tricky error handling is done in that section of the code
base.

`IOSim` exposes a detailed trace, which can be enhanced by labelling threads,
or mutable variables, tracing `Dynamic` values (which can be recovered from the
trace) or simple `String` based tracing.  Although its agnostic with respect to
the logging framework, it worked of us particularly well using
[contra-tracer][contra-tracer].  It has been used to develop, test and debug
a complex, highly-concurrent, distributed system
([ouroboros-network][ouroboros-network]), in particular

* write network simulations, to verify a complex networking stack;
* write disk IO simulations, to verify a database implementation.

### Supporting material

* [Philipp Kant (@kantp) at Bobconf 2022][bob-conf]


## Packages

* `io-sim`: provides two simulator interpreters: `IOSim` and `IOSimPOR` - an
    enhanced `IOSim` version with schedule discovery capabilities.
* `io-classes`: class bases interface, which allows to to abstract over the
    monad
* `strict-stm`: strict STM operations


## Differences from `base`, `async` or `exceptions` packages

### Major differences

* `threadDelay` is using `DiffTime` (which is measured in _seconds_ rather than _microseconds_).
* `regiterDelay` is using `DiffTime`
* `timeout` is using `DiffTime`
* `getMonotonicTime` returns `Time` (a newtype wrapper around `DiffTime`)


### Minor differences

Some of the types have more general kind signatures, e.g.

```
type Async :: (Type -> Type) -> Type -> Type
```

The first type of kind `Type -> Type` describes the monad which could be
instantiated to `IO`, `IOSim` or some other monad stack build with monad
transformers.  The same applies to many other types, e.g. `TVar`, `TMVar`.

The following types although similar to the originals are not the same as the
ones that come from `base`, `async`, or `excpetions` packages:

* `Handler` (origin: `base`)
* `MaskingState` (origin: `base`)
* `Concurrently` (origin: `async`)
* `ExceptionInLinkedThread` (origin: `async`): `io-class`es version does not
  store `Async`
* `ExitCase` (origin: `exceptions`)


### Issues

New issues should be reported in this repository, we still have a list
of issues opened in the `ouroboros-network` repository:

* [io-sim issues][io-sim-issues]
* [io-classes issues][io-sim-issues]

[io-sim-por-how-to]: ./io-sim/how-to-use-IOSimPOR.md
[ouroboros-network]: https://github.com/input-output-hk/ouroboros-network
[io-sim-issues]: https://github.com/input-output-hk/ouroboros-network/issues?q=is%3Aopen+is%3Aissue+label%3Aio-sim
[io-classes-issues]: https://github.com/input-output-hk/ouroboros-network/issues?q=is%3Aopen+is%3Aissue+label%3Aio-classes
[contra-tracer]: https://hackage.haskell.org/package/contra-tracer
[io-sim-por]: https://github.com/input-output-hk/io-sim/blob/main/io-sim/how-to-use-IOSimPOR.md
[bob-conf]: https://www.youtube.com/watch?v=e74TE0b4xEM&t=13662s
