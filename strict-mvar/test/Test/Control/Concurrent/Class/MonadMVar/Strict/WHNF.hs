{-# LANGUAGE CPP              #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Test.Control.Concurrent.Class.MonadMVar.Strict.WHNF (tests) where

#if CHECKED
import           Control.Concurrent.Class.MonadMVar.Strict.Checked
#else
import           Control.Concurrent.Class.MonadMVar.Strict
#endif
import           Control.Monad (void)
import           Data.Typeable (Proxy (..), Typeable)
import           NoThunks.Class (OnlyCheckWhnf (OnlyCheckWhnf), unsafeNoThunks)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, monitor, run)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Fun, applyFun, counterexample,
                     testProperty)
import           Test.Utils (monadicSim)

{-------------------------------------------------------------------------------
  Main test tree
-------------------------------------------------------------------------------}

name :: String
#if CHECKED
name = "Strict.Checked"
#else
name = "Strict"
#endif

tests :: TestTree
tests = testGroup ("Test.Control.Concurrent.Class.MonadMVar." <> name) [
      testGroup "WHNF" [
          testGroup "IO" [
              testProperty "prop_newMVar" $
                monadicIO .: prop_newMVar (Proxy @Int)
            , testProperty "prop_newMVarWithInvariant" $
                monadicIO .: prop_newMVarWithInvariant (Proxy @Int)
            , testProperty "prop_putMVar" $
                monadicIO .: prop_putMVar (Proxy @Int)
            , testProperty "prop_swapMVar" $
                monadicIO .: prop_swapMVar (Proxy @Int)
            , testProperty "prop_tryPutMVar" $
                monadicIO .: prop_tryPutMVar (Proxy @Int)
            , testProperty "prop_modifyMVar_" $
                monadicIO .: prop_modifyMVar_ (Proxy @Int)
            , testProperty "prop_modifyMVar" $
                monadicIO .: prop_modifyMVar (Proxy @Int) (Proxy @Char)
            , testProperty "prop_modifyMVarMasked_" $
                monadicIO .: prop_modifyMVarMasked_ (Proxy @Int)
            , testProperty "prop_modifyMVarMasked" $
                monadicIO .: prop_modifyMVarMasked (Proxy @Int) (Proxy @Char)
            ]
        , testGroup "IOSim" [
              testProperty "prop_newMVar" $ \x f ->
                monadicSim $ prop_newMVar (Proxy @Int) x f
            , testProperty "prop_newMVarWithInvariant" $ \x f ->
                monadicSim $ prop_newMVarWithInvariant (Proxy @Int) x f
            , testProperty "prop_putMVar" $ \x f ->
                monadicSim $ prop_putMVar (Proxy @Int) x f
            , testProperty "prop_swapMVar" $ \x f ->
                monadicSim $ prop_swapMVar (Proxy @Int) x f
            , testProperty "prop_tryPutMVar" $ \x f ->
                monadicSim $ prop_tryPutMVar (Proxy @Int) x f
            , testProperty "prop_modifyMVar_" $ \x f ->
                monadicSim $ prop_modifyMVar_ (Proxy @Int) x f
            , testProperty "prop_modifyMVar" $ \x f ->
                monadicSim $ prop_modifyMVar (Proxy @Int) (Proxy @Char) x f
            , testProperty "prop_modifyMVarMasked_" $ \x f ->
                monadicSim $ prop_modifyMVarMasked_ (Proxy @Int) x f
            , testProperty "prop_modifyMVarMasked" $ \x f ->
                monadicSim $ prop_modifyMVarMasked (Proxy @Int) (Proxy @Char) x f
            ]
        ]
    ]

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

infixr 9 .:

(.:) :: (y -> z) -> (x0 -> x1 -> y) -> (x0 -> x1 -> z)
(.:) g f x0 x1 = g (f x0 x1)

isInWHNF :: (MonadMVar m, Typeable a) => StrictMVar m a -> PropertyM m Bool
isInWHNF v = do
    x <- run $ readMVar v
    let tinfoMay = unsafeNoThunks (OnlyCheckWhnf x)
    case tinfoMay of
      Nothing    -> pure True
      Just tinfo -> monitor (counterexample $ "Thunk found: " ++ show tinfo)
                 >> pure False

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

prop_newMVar ::
     (MonadMVar m, Typeable a)
  => proxy a
  -> a
  -> Fun a a
  -> PropertyM m Bool
prop_newMVar _ x f = do
    v <- run $ newMVar (applyFun f x)
    isInWHNF v

prop_newMVarWithInvariant ::
     (MonadMVar m, Typeable a)
  => proxy a
  -> a
  -> Fun a a
  -> PropertyM m Bool
prop_newMVarWithInvariant _ x f = do
    v <- run $ newMVarWithInvariant (const Nothing) (applyFun f x)
    isInWHNF v

prop_putMVar ::
     (MonadMVar m, Typeable a)
  => proxy a
  -> a
  -> Fun a a
  -> PropertyM m Bool
prop_putMVar _ x f = do
    v <- run newEmptyMVar
    run $ putMVar v (applyFun f x)
    isInWHNF v

prop_swapMVar ::
     (MonadMVar m, Typeable a)
  => proxy a
  -> a
  -> Fun a a
  -> PropertyM m Bool
prop_swapMVar _ x f = do
    v <- run $ newMVar x
    void $ run $ swapMVar v (applyFun f x)
    isInWHNF v

prop_tryPutMVar ::
     (MonadMVar m, Typeable a)
  => proxy a
  -> a
  -> Fun a a
  -> PropertyM m Bool
prop_tryPutMVar _ x f = do
    v <- run newEmptyMVar
    b <- run $ tryPutMVar v (applyFun f x)
    b' <- isInWHNF v
    pure (b && b')

prop_modifyMVar_ ::
     (MonadMVar m, Typeable a)
  => Proxy a
  -> a
  -> Fun a a
  -> PropertyM m Bool
prop_modifyMVar_ _ x f =do
    v <-  run $ newMVar x
    run $ modifyMVar_ v (pure . applyFun f)
    isInWHNF v

prop_modifyMVar ::
     (MonadMVar m, Typeable a)
  => Proxy a
  -> Proxy b
  -> a
  -> Fun a (a, b)
  -> PropertyM m Bool
prop_modifyMVar _ _ x f =do
    v <-  run $ newMVar x
    void $ run $ modifyMVar v (pure . applyFun f)
    isInWHNF v

prop_modifyMVarMasked_ ::
     (MonadMVar m, Typeable a)
  => Proxy a
  -> a
  -> Fun a a
  -> PropertyM m Bool
prop_modifyMVarMasked_ _ x f =do
    v <-  run $ newMVar x
    void $ run $ modifyMVarMasked_ v (pure . applyFun f)
    isInWHNF v

prop_modifyMVarMasked ::
     (MonadMVar m, Typeable a)
  => Proxy a
  -> Proxy b
  -> a
  -> Fun a (a, b)
  -> PropertyM m Bool
prop_modifyMVarMasked _ _ x f =do
    v <-  run $ newMVar x
    void $ run $ modifyMVarMasked v (pure . applyFun f)
    isInWHNF v
