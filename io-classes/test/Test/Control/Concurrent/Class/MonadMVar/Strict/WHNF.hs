{-# OPTIONS_GHC -fno-ignore-asserts #-}

-- | Test whether functions on 'StrictMVar's correctly force values to WHNF
-- before they are put inside the 'StrictMVar'.
module Test.Control.Concurrent.Class.MonadMVar.Strict.WHNF
  ( prop_newMVar
  , prop_putMVar
  , prop_swapMVar
  , prop_tryPutMVar
  , prop_modifyMVar_
  , prop_modifyMVar
  , prop_modifyMVarMasked_
  , prop_modifyMVarMasked
  , (.:)
  ) where

import Control.Concurrent.Class.MonadMVar.Strict
import Control.Exception (assert)
import Control.Monad (void)
import Data.Typeable (Typeable)
import NoThunks.Class (OnlyCheckWhnf (OnlyCheckWhnf), unsafeNoThunks)
import Test.QuickCheck

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

infixr 9 .:

(.:) :: (y -> z) -> (x0 -> x1 -> y) -> (x0 -> x1 -> z)
(.:) g f x0 x1 = g (f x0 x1)

isInWHNF :: (MonadMVar m, Typeable a) => StrictMVar m a -> m Property
isInWHNF v = do
    x <- readMVar v
    case unsafeNoThunks (OnlyCheckWhnf x) of
      Nothing    -> pure (property True)
      Just tinfo -> pure (counterexample ("Not in WHNF: " ++ show tinfo) (property False))

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

prop_newMVar ::
     MonadMVar m
  => Int
  -> Fun Int Int
  -> m Property
prop_newMVar x f = do
    v <- newMVar (applyFun f x)
    isInWHNF v

prop_putMVar ::
     MonadMVar m
  => Int
  -> Fun Int Int
  -> m Property
prop_putMVar x f = do
    v <- newEmptyMVar
    putMVar v (applyFun f x)
    isInWHNF v

prop_swapMVar ::
     MonadMVar m
  => Int
  -> Fun Int Int
  -> m Property
prop_swapMVar x f = do
    v <- newMVar x
    void $ swapMVar v (applyFun f x)
    isInWHNF v

prop_tryPutMVar ::
     MonadMVar m
  => Int
  -> Fun Int Int
  -> m Property
prop_tryPutMVar x f = do
    v <- newEmptyMVar
    b <- tryPutMVar v (applyFun f x)
    assert b $ pure ()
    isInWHNF v

prop_modifyMVar_ ::
     MonadMVar m
  => Int
  -> Fun Int Int
  -> m Property
prop_modifyMVar_ x f = do
    v <-  newMVar x
    modifyMVar_ v (pure . applyFun f)
    isInWHNF v

prop_modifyMVar ::
     MonadMVar m
  => Int
  -> Fun Int (Int, Char)
  -> m Property
prop_modifyMVar x f = do
    v <- newMVar x
    void $ modifyMVar v (pure . applyFun f)
    isInWHNF v

prop_modifyMVarMasked_ ::
     MonadMVar m
  => Int
  -> Fun Int Int
  -> m Property
prop_modifyMVarMasked_ x f = do
    v <- newMVar x
    modifyMVarMasked_ v (pure . applyFun f)
    isInWHNF v

prop_modifyMVarMasked ::
     MonadMVar m
  => Int
  -> Fun Int (Int, Char)
  -> m Property
prop_modifyMVarMasked x f = do
    v <-  newMVar x
    void $ modifyMVarMasked v (pure . applyFun f)
    isInWHNF v
