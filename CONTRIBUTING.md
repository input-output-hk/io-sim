# Contributing to io-sim

We designed `io-classes` to be as close as possible to what `base` package
provides.  Almost all `IO` instances instantiate with api provided by one of
the core packages, see
[example](https://github.com/input-output-hk/io-sim/blob/main/io-classes/src/Control/Monad/Class/MonadSTM.hs?plain=1#L410-L446).
Please keep this in mind when adding new functionality.

## Style Guide

Please follow the [style guide][style-guide].

## Pull Request

Ideally the series of commits you want to include should be written with the
reviewer in mind.  This will make it easier for the maintainers to review your
changes.  Each commit should heave a clear explanation what it changes and most
importantly why the changes are necessary.   We prefer if commits are small and
just introduce a single change.  For example if you are adding a new typeclass
in `io-classes` and you want `io-sim` and `io-sim-por` to support it, we'd
expect at least three commits: first which introduces the new type class, and
separate commits which add implement it for `IOSim` and `IOSimPOR`, since both
are using the same free monad, just interpret it differently, it's ok to first
implement it for `IOSim` and leave placeholders in the `IOSimPOR` interpreter.

If you are adding a new functionality to `MonadSTM`, don't forget to support it
in `strict-stm` package.

Please also include your changes in `CHANGELOG.md` file.


[style-guide]: https://github.com/input-output-hk/ouroboros-network/blob/master/docs/StyleGuide.md

