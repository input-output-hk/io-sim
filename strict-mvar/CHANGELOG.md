# Revsion history of strict-mvar

## next version

### Breaking changes

* Remove the `asserts` package flag.

### Non breaking changes

* Add a `StrictMVar` with invariant checking in
  `Control.Concurrent.Class.MonadMVar.Strict.Checked`.
* Make the checked/unchecked `StrictMVar` modules drop-in replacements of one
  another by unifying the interfaces. As a result,
  `Control.Concurrent.Class.MonadMVar.Strict` now has `newMVarWithInvariant` and
  `newEmptyMVarWithInvariant` functions that ignore the invariant argument.

## 1.1.0.0

### Non breaking changes

* `io-classes-1.1.0.0`

## 1.0.0.1

### Non breaking changes

* Support `ghc-9.6`.

## 1.0.0.0

## 0.1.0.0

* Initial version, not released on Hackage.
