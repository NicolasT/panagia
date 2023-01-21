{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Panagia.Quorum.Benchmark (benchmarks) where

import Criterion (Benchmark, bench, bgroup, whnf)
import Data.Set (Set)
import Panagia.Quorum
  ( simpleMajority,
    simpleMajority',
    weighted,
    weighted',
  )

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Panagia.Quorum"
    [ bgroup
        "simpleQuorum"
        [ bench (show n) (whnf (simpleMajority nodes) [1 .. n]) | n <- [1 .. l]
        ],
      bgroup
        "simpleQuorum'"
        [ bench (show n) (whnf (simpleMajority' nodes) [1 .. n]) | n <- [1 .. l]
        ],
      bgroup
        "weighted"
        [ bench (show n) (whnf (weighted getWeight nodes) [1 .. n]) | n <- [1 .. l]
        ],
      bgroup
        "weighted'"
        [ bench (show n) (whnf (weighted' getWeight nodes) [1 .. n]) | n <- [1 .. l]
        ]
    ]
  where
    nodes :: Set Word
    nodes = [1 .. 5]
    l = fromIntegral (length nodes)
    getWeight = \case
      1 -> (2 :: Word)
      2 -> 2
      3 -> 2
      4 -> 3
      5 -> 3
      _ -> error "Invalid node"
