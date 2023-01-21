{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Panagia.Quorum.Test.SMT (tests) where

import Data.SBV (constrain, isTheorem, sbvForall, (./=))
import Data.SBV.Set (empty, intersection, isSubsetOf, universal)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "Panagia.Quorum"
    [ testTheorems
    ]

testTheorems :: TestTree
testTheorems =
  testGroup
    "Theorems"
    [ testCase "quorumNonEmpty" $
        assertTheorem $ do
          q1 <- sbvForall "q1"
          constrain $ quorumAssumption1 q1
          constrain =<< do
            q2 <- sbvForall "q2"
            constrain $ quorumAssumption1 q2
            pure $ quorumAssumption2 q1 q2
          pure $ q1 ./= empty @Integer
    ]
  where
    quorumAssumption1 q = q `isSubsetOf` universal
    quorumAssumption2 q1 q2 = q1 `intersection` q2 ./= empty
    assertTheorem t = assertBool "Theorem can't be proven" =<< isTheorem t
