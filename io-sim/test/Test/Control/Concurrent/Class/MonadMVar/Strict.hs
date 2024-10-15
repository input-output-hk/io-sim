module Test.Control.Concurrent.Class.MonadMVar.Strict where

import Control.Monad.IOSim
import Test.Control.Concurrent.Class.MonadMVar.Strict.WHNF

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic (run)

tests :: TestTree
tests = testGroup "Test.Control.Concurrent.Class.MonadMVar.Strict"
        [ testGroup "WHNF"
          [ testGroup "IOSIM"
            [ testProperty "prop_newMVar" $ \x f ->
                monadicIOSim_ $ run $ prop_newMVar x f
            , testProperty "prop_putMVar" $ \x f ->
                monadicIOSim_ $ run $ prop_putMVar x f
            , testProperty "prop_swapMVar" $ \x f ->
                monadicIOSim_ $ run $ prop_swapMVar x f
            , testProperty "prop_tryPutMVar" $ \x f ->
                monadicIOSim_ $ run $ prop_tryPutMVar x f
            , testProperty "prop_modifyMVar_" $ \x f ->
                monadicIOSim_ $ run $ prop_modifyMVar_ x f
            , testProperty "prop_modifyMVar" $ \x f ->
                monadicIOSim_ $ run $ prop_modifyMVar x f
            , testProperty "prop_modifyMVarMasked_" $ \x f ->
                monadicIOSim_ $ run $ prop_modifyMVarMasked_ x f
            , testProperty "prop_modifyMVarMasked" $ \x f ->
                monadicIOSim_ $ run $ prop_modifyMVarMasked x f
            ]
          ]
        ]
