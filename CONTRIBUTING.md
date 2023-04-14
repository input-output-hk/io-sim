# Building

The project is build with `cabal-install`.  You might need to run `cabal
update` after cloning the repository (to update [`Cardano Haskell
Packages`][CHaP] (`ChaP`) index).

# Design Principles

We designed `io-classes` to be as close as possible to what `base` package
provides.  Almost all `IO` instances instantiate with api provided by one of
the core packages, see
[example](https://github.com/input-output-hk/io-sim/blob/main/io-classes/src/Control/Monad/Class/MonadSTM.hs?plain=1#L410-L446).
Please keep this in mind when adding new functionality.

# Using in your project

Currently the package is published to [`CHaP`][CHaP].  In future it will be
published to `Hackage`.  If you want to pull it from [`CHaP`][CHaP], this is
relatively easy to setup; for example, checkout the
[`cabal.project`](https://github.com/input-output-hk/typed-protocols/blob/master/cabal.project)
file.  Alternatively, you can relay on `source-repository-package` stanza in
a `cabal.project` file.

# Testing

The `typed-protocols-examples` package contains two simple protocols `PingPong`
& `ReqResp` which are used for illustration & testing.

```
cabal run typed-protocols-examples:test
```

If you contribute a more complex feature, it might be a good idea to run the
tests suites from the `ouroboros-network` repository, especially:

* `network-mux:test`
* `ouroboros-network-framework:test`
* `ouroboros-network:test`

If you've been working on `IOSimPOR`, please enable the `nightly` cabal flag in
the `ouroboros-network-testing` framework.  To configure `ouroboros-network`
repository with the right `io-sim` you can include the following snippet in
`cabal.project.local` file:
```
source-repository-package
  type: git
  location: <local .git directory; ssh or http url>
  tag: <hash of the commit, branch name, etc>
  subdir:
    io-classes
    io-classes-mtl
    io-sim
    si-timers
    strict-stm
    strict-mvar

package ouroboros-network-testing
  flags: +nightly
```

# Code Style

Please follow local style.  For a more detailed style guide see
[link](https://github.com/input-output-hk/ouroboros-network/blob/master/docs/StyleGuide.md).

# Pull Requests

Each commit shall be small and preferably address one thing at a time.  Well
organised & documented commits make it much easier for the maintainers to
review them.

New features should be well documented & tested, which means including new
tests as necessary.  You might be asked by the maintainers to write & include
additional tests.

Each commit should build & test, at least the package you are changing.  You
can update other packages from this repository in a subsequent commit.

Please use a draft PRs if the work is still in progress.

We require all commits to be signed, see [this guide][gh-signing-commits].

If your pull requests resolves an existing issue, please link your PR to that
issue, see [GitHub documentation][gh-link-issue].

Please include your changes in `CHANGELOG.md` files (per package).

## MonadSTM features

If you are adding a new functionality to `MonadSTM`, don't forget to support it
in `strict-stm` package.

# Releases

The major version of `io-sim`, `io-classes` and `strict-stm` packages are kept
in sync.  This means that if any of the packages introduces a breaking change
all major version SHOULD be bumped.  The minor versions are kept independent.

The drawback is that if you declare `io-classes ^>= 0.x` then you will need to
bump it when new version of `io-sim` is published (even if there are no changes
in `io-classes`).  The con is that you just need to declare version of
`io-classes` to have a consistent ecosystem of the three packages.

# Tips

## `ppTrace` is strict

Both `ppTrace` and `ppTrace_` are strict.  They evaluate the trace before they
produce any result, thus they are not useful when your trace diverges.  This
can happen if evaluation encounters unhandled exception e.g. one of assertion
fires (either internal or otherwise).  In that case, instead of `ppTrace` you
can use `Data.Trace.toList` and simply `traverse print` the list.  This will
give you the trace up to the point of failure.

## `IOSim` and `STMSim` monads are based on lazy `ST` monad

This means that any action is forced only when the result is needed.  This is
more lazy than `IO` monad.  Thus if you want to use `Debug.Trace.traceM` inside
`schedule` function you need to:
```hs
    ...
    !_ <- Debug.Trace.traceM "hello"
    ...
```



[CHaP]: https://github.com/input-output-hk/cardano-haskell-packages/
[gh-link-issue]: https://docs.github.com/en/github/managing-your-work-on-github/linking-a-pull-request-to-an-issue
[gh-signing-commits]: https://docs.github.com/en/authentication/managing-commit-signature-verification/signing-commits


