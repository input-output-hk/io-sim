{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- | Test whether functions on 'StrictMVar's correctly force values to WHNF
-- before they are put inside the 'StrictMVar'.
module Test.Control.Concurrent.Class.MonadMVar.Strict.WHNF (tests) where

#if TEST_CHECKED
import           Control.Concurrent.Class.MonadMVar.Strict.Checked
#else
import           Control.Concurrent.Class.MonadMVar.Strict
#endif
import           Control.Monad (void)
import           Data.Typeable (Typeable)
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
#if TEST_CHECKED
name = "Strict.Checked"
#else
name = "Strict"
#endif

tests :: TestTree
tests = testGroup ("Test.Control.Concurrent.Class.MonadMVar." <> name) [
      testGroup "WHNF" [
          testGroup "IO" [
              testProperty "prop_newMVar" $
                monadicIO .: prop_newMVar
            , testProperty "prop_newMVarWithInvariant" $
                monadicIO .: prop_newMVarWithInvariant
            , testProperty "prop_putMVar" $
                monadicIO .: prop_putMVar
            , testProperty "prop_swapMVar" $
                monadicIO .: prop_swapMVar
            , testProperty "prop_tryPutMVar" $
                monadicIO .: prop_tryPutMVar
            , testProperty "prop_modifyMVar_" $
                monadicIO .: prop_modifyMVar_
            , testProperty "prop_modifyMVar" $
                monadicIO .: prop_modifyMVar
            , testProperty "prop_modifyMVarMasked_" $
                monadicIO .: prop_modifyMVarMasked_
            , testProperty "prop_modifyMVarMasked" $
                monadicIO .: prop_modifyMVarMasked
            ]
        , testGroup "IOSim" [
              testProperty "prop_newMVar" $ \x f ->
                monadicSim $ prop_newMVar x f
            , testProperty "prop_newMVarWithInvariant" $ \x f ->
                monadicSim $ prop_newMVarWithInvariant x f
            , testProperty "prop_putMVar" $ \x f ->
                monadicSim $ prop_putMVar x f
            , testProperty "prop_swapMVar" $ \x f ->
                monadicSim $ prop_swapMVar x f
            , testProperty "prop_tryPutMVar" $ \x f ->
                monadicSim $ prop_tryPutMVar x f
            , testProperty "prop_modifyMVar_" $ \x f ->
                monadicSim $ prop_modifyMVar_ x f
            , testProperty "prop_modifyMVar" $ \x f ->
                monadicSim $ prop_modifyMVar x f
            , testProperty "prop_modifyMVarMasked_" $ \x f ->
                monadicSim $ prop_modifyMVarMasked_ x f
            , testProperty "prop_modifyMVarMasked" $ \x f ->
                monadicSim $ prop_modifyMVarMasked x f
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

prop_newMVarWithInvariant ::
     MonadMVar m
  => Int
  -> Fun Int Int
  -> PropertyM m Bool
prop_newMVarWithInvariant x f = do
    v <- run $ newMVarWithInvariant (const Nothing) (applyFun f x)
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
