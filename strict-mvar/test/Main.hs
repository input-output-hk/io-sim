module Main where

import qualified Test.Control.Concurrent.Class.MonadMVar.Strict.Checked as Checked
import qualified Test.Control.Concurrent.Class.MonadMVar.Strict.WHNF as WHNF
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "strict-mvar" [
      Checked.tests
    , WHNF.tests
    ]
