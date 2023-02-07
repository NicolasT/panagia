module Main (main) where

import qualified Panagia.Paxos.SingleDecree.Test as PPS
import qualified Panagia.Quorum.Test as PQ
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ PQ.tests,
        PPS.tests
      ]
