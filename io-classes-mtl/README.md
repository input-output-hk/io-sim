# Experimental MTL Instance for io-classes

`ReaderT` instances are included in `io-classes`, but all other instances are
included in this package.  Some of them are rather novel and experimental
others might be less so.   This code is not well tested, and some of it hasn't run
in a production environment as we know (let us know if you do!).

The `MonadSTM` instances for monad transformers are somewhat novel.  The `STM`
monad is transformed together with the base monad.  This means that the
transformer primitive operations are available in `STM`.  For example you an
`STM` transaction can lock updating the state of the current thread.

We haven't included `MonadAsync` instances (although we have an experimental
branch how this could be done).  It could work like the `lifted-async`
package.  But we feel this can be controversial, so it's not included.

The design goal is to follow  `exception` package instances, but since we don't
have any tests we are not very confident of this either.  Contributions are
welcomed!
