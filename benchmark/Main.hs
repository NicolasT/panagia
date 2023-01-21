module Main (main) where

import Criterion.Main (defaultMain)
import qualified Panagia.Quorum.Benchmark as PQ

main :: IO ()
main =
  defaultMain
    [ PQ.benchmarks
    ]
