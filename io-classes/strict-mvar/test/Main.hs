module Main where

import Test.Control.Concurrent.Class.MonadMVar.Strict.WHNF

import Test.QuickCheck.Monadic (monadicIO)
import Test.Tasty
import Test.Tasty.QuickCheck


main :: IO ()
main = defaultMain $ testGroup "strict-mvar" [tests]

tests :: TestTree
tests = testGroup "Test.Control.Concurrent.Class.MonadMVar.Strict"
        [ testGroup "WHNF"
          [ testGroup "IO"
            [ testProperty "prop_newMVar" $
                monadicIO .: prop_newMVar
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
          ]
        ]
