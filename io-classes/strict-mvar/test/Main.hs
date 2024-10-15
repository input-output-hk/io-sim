module Main where

import Test.Control.Concurrent.Class.MonadMVar.Strict.WHNF

import Test.Tasty
import Test.Tasty.QuickCheck


main :: IO ()
main = defaultMain $ testGroup "strict-mvar" [tests]

tests :: TestTree
tests = testGroup "Test.Control.Concurrent.Class.MonadMVar.Strict"
        [ testGroup "WHNF"
          [ testGroup "IO"
            [ testProperty "prop_newMVar" $
                ioProperty .: prop_newMVar
            , testProperty "prop_putMVar" $
                ioProperty .: prop_putMVar
            , testProperty "prop_swapMVar" $
                ioProperty .: prop_swapMVar
            , testProperty "prop_tryPutMVar" $
                ioProperty .: prop_tryPutMVar
            , testProperty "prop_modifyMVar_" $
                ioProperty .: prop_modifyMVar_
            , testProperty "prop_modifyMVar" $
                ioProperty .: prop_modifyMVar
            , testProperty "prop_modifyMVarMasked_" $
                ioProperty .: prop_modifyMVarMasked_
            , testProperty "prop_modifyMVarMasked" $
                ioProperty .: prop_modifyMVarMasked
            ]
          ]
        ]
