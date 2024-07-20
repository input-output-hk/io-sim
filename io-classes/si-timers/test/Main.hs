module Main (main) where

import Test.Tasty

import Test.MonadTimer qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "io-classes"
    [ Test.MonadTimer.tests
    ]

