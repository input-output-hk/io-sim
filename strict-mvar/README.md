# Strict Mutable Variables

The `strict-mvar` package provides a strict interface to mutable variables
(`MVar`). It builds on top of `io-classes`, and thus it provides the interface
for `MVar`s implementations from both
[base](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Concurrent-MVar.html)
and [io-sim](https://github.com/input-output-hk/io-sim).

## Checked and unchecked `StrictMVar`s

There are currently two variant implementations of `StrictMVar`s in this package:
* `Control.Concurrent.Class.MonadMVar.Strict`
* `Control.Concurrent.Class.MonadMVar.Strict.Checked`

The _unchecked_ module provides the simplest implementation of a `StrictMVar`: a
light wrapper around lazy MVars that forces values to WHNF before they are put
into the MVar. The _checked_ module does the exact same thing, but it has the
additional feature that the user can provide an invariant that is checked each
time a new value is placed inside the MVar. The two modules are drop-in
replacements for one another: switching from `*.Strict` to `*.Strict.Checked`
will enable invariant checking, while the converse will disable invariant
checking. To facilitate drop-in replacement, both modules share the same
interface, though in case of the `*.Strict` module, everything related to
invariants will be ignored. This will be explicitly mentioned in the Haddock
documentation of said definitions. For example:

```haskell
-- | The given invariant will never be checked. 'newMVarWithInvariant' is a
-- light wrapper around 'newMVar', and is only included here to ensure that the
-- current module and "Control.Concurrent.Class.MonadMVar.Strict.Checked" are
-- drop-in replacements for one another.
newMVarWithInvariant :: MonadMVar m
                     => (a -> Maybe String)
                     -> a
                     -> m (StrictMVar m a)
```

**Note:** though the two modules are drop-in replacements for one another, the
`StrictMVar` type from `*.Strict` and the `StrictMVar` type from
`*.Strict.Checked` do not share the same internal representation, and so they
are distinct types.

## Guarantees for invariant checking

Although all functions that modify a checked `StrictMVar` will check the
invariant, we do *not* guarantee that the value inside the `StrictMVar` always
satisfies the invariant. Instead, we *do* guarantee that if the `StrictMVar` is
updated with a value that does not satisfy the invariant, an exception is thrown
*after* the new value is written to the `StrictMVar`. The reason for this weaker
guarantee is that leaving an `MVar` empty can lead to very hard to debug
"blocked indefinitely" problems.