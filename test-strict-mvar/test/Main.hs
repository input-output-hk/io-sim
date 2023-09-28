module Main where

import Test.Control.Concurrent.Class.MonadMVar.Strict.WHNF qualified as WHNF
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "strict-mvar" [
      WHNF.tests
    ]
