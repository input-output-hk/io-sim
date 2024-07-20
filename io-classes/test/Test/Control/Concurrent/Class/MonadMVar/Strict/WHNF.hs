{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
import Control.Monad (void)
import Data.Typeable (Typeable)
import NoThunks.Class (OnlyCheckWhnf (OnlyCheckWhnf), unsafeNoThunks)
import Test.QuickCheck
import Test.QuickCheck.Monadic (PropertyM, monitor, run)

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

infixr 9 .:

(.:) :: (y -> z) -> (x0 -> x1 -> y) -> (x0 -> x1 -> z)
(.:) g f x0 x1 = g (f x0 x1)

isInWHNF :: (MonadMVar m, Typeable a) => StrictMVar m a -> PropertyM m Bool
isInWHNF v = do
    x <- run $ readMVar v
    case unsafeNoThunks (OnlyCheckWhnf x) of
      Nothing    -> pure True
      Just tinfo -> monitor (counterexample $ "Not in WHNF: " ++ show tinfo)
                 >> pure False

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

prop_newMVar ::
     MonadMVar m
  => Int
  -> Fun Int Int
  -> PropertyM m Bool
prop_newMVar x f = do
    v <- run $ newMVar (applyFun f x)
    isInWHNF v

prop_putMVar ::
     MonadMVar m
  => Int
  -> Fun Int Int
  -> PropertyM m Bool
prop_putMVar x f = do
    v <- run newEmptyMVar
    run $ putMVar v (applyFun f x)
    isInWHNF v

prop_swapMVar ::
     MonadMVar m
  => Int
  -> Fun Int Int
  -> PropertyM m Bool
prop_swapMVar x f = do
    v <- run $ newMVar x
    void $ run $ swapMVar v (applyFun f x)
    isInWHNF v

prop_tryPutMVar ::
     MonadMVar m
  => Int
  -> Fun Int Int
  -> PropertyM m Bool
prop_tryPutMVar x f = do
    v <- run newEmptyMVar
    b <- run $ tryPutMVar v (applyFun f x)
    b' <- isInWHNF v
    pure (b && b')

prop_modifyMVar_ ::
     MonadMVar m
  => Int
  -> Fun Int Int
  -> PropertyM m Bool
prop_modifyMVar_ x f =do
    v <-  run $ newMVar x
    run $ modifyMVar_ v (pure . applyFun f)
    isInWHNF v

prop_modifyMVar ::
     MonadMVar m
  => Int
  -> Fun Int (Int, Char)
  -> PropertyM m Bool
prop_modifyMVar x f =do
    v <-  run $ newMVar x
    void $ run $ modifyMVar v (pure . applyFun f)
    isInWHNF v

prop_modifyMVarMasked_ ::
     MonadMVar m
  => Int
  -> Fun Int Int
  -> PropertyM m Bool
prop_modifyMVarMasked_ x f =do
    v <-  run $ newMVar x
    void $ run $ modifyMVarMasked_ v (pure . applyFun f)
    isInWHNF v

prop_modifyMVarMasked ::
     MonadMVar m
  => Int
  -> Fun Int (Int, Char)
  -> PropertyM m Bool
prop_modifyMVarMasked x f =do
    v <-  run $ newMVar x
    void $ run $ modifyMVarMasked v (pure . applyFun f)
    isInWHNF v
