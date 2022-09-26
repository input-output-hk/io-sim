{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This module corresponds to `Control.Concurrent.STM.TArray` in "stm" package
--
module Control.Concurrent.Class.MonadSTM.Strict.TArray
  ( StrictTArray
  , LazyTArray
  , toLazyTArray
  , fromLazyTArray
  ) where


import qualified Control.Concurrent.Class.MonadSTM.TArray as Lazy

import           Data.Array.Base (MArray (..))


type LazyTArray  m = Lazy.TArray m

newtype StrictTArray m i e = StrictTArray { toLazyTArray :: LazyTArray m i e }

fromLazyTArray :: LazyTArray m i e -> StrictTArray m i e
fromLazyTArray = StrictTArray

instance ( MArray (Lazy.TArray m) e stm
         , Monad stm
         )
      => MArray (StrictTArray m) e stm where
    getBounds (StrictTArray arr) = getBounds arr
    newArray  b !e = StrictTArray <$> newArray b e
    newArray_ b    = StrictTArray <$> newArray_ b
    unsafeRead     (StrictTArray arr) i    = unsafeRead arr i
    unsafeWrite    (StrictTArray arr) i !e = unsafeWrite arr i e
    getNumElements (StrictTArray arr)      = getNumElements arr

