module Main (main) where

import qualified Panagia.Quorum.Test.SMT as PQ
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "SMT Tests"
      [ PQ.tests
      ]
