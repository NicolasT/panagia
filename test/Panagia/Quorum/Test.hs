{-# LANGUAGE FlexibleContexts #-}

module Panagia.Quorum.Test (tests) where

import Data.Foldable (foldrM)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Panagia.Quorum
  ( prop_quorumAssumption1,
    prop_quorumAssumption2,
    prop_quorumNonEmpty,
    simpleMajority,
    simpleMajority',
    weighted,
    weighted',
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    classify,
    forAll,
    getPositive,
    suchThat,
    testProperty,
    (.&&.),
    (=/=),
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "Panagia.Quorum"
    [ testSimpleQuorum,
      testSimpleQuorum',
      testWeighted,
      testWeighted'
    ]

randomSimpleQuorumAcceptors :: Gen (Set Word)
randomSimpleQuorumAcceptors = arbitrary `suchThat` isNotEmpty

testSimpleQuorum :: TestTree
testSimpleQuorum =
  testGroup
    "simpleQuorum"
    [ testGroup "Cases" $
        testCases
          simpleMajority
          [ ([1 :: Int .. 3], [1, 2], True),
            ([1 .. 3], [1], False),
            ([1 .. 3], [2, 3, 4], False)
          ],
      testGroup "Properties" $
        properties randomSimpleQuorumAcceptors simpleMajority (fromIntegral . Set.size),
      testProperty "length quorum >= length acceptors `div` 2 + 1" $ do
        forAll randomSimpleQuorumAcceptors $ \acceptors ->
          forAll (subsetOf acceptors) $ \maybeQuorum ->
            let isQuorum = simpleMajority acceptors maybeQuorum
             in classify isQuorum "quorum" $
                  isQuorum === (length maybeQuorum >= length acceptors `div` 2 + 1)
    ]

testSimpleQuorum' :: TestTree
testSimpleQuorum' =
  testGroup
    "simpleQuorum'"
    [ testProperty "simpleQuorum' a q == simpleQuorum a q" $ do
        forAll randomSimpleQuorumAcceptors $ \acceptors ->
          forAll (subsetOf acceptors) $ \maybeQuorum ->
            let isQuorum = simpleMajority acceptors maybeQuorum
             in classify isQuorum "quorum" $
                  simpleMajority' acceptors maybeQuorum === isQuorum
    ]

randomWeightedAcceptors :: Gen (Set (Word, Word))
randomWeightedAcceptors = do
  Map.foldrWithKey (\k w s -> Set.insert (k, getPositive w) s) mempty <$> (arbitrary `suchThat` isNotEmpty)

testWeighted :: TestTree
testWeighted =
  testGroup
    "weighted"
    [ testGroup "Cases" $
        testCases
          (weighted snd)
          [ ([(1 :: Int, 3 :: Int), (2, 1), (3, 1)], [(1, 3)], True),
            ([(1, 3), (2, 1), (3, 1)], [(2, 1), (3, 1)], False)
          ],
      testGroup "Properties" $
        properties randomWeightedAcceptors (weighted snd) (Set.foldl' (\a (_, w) -> w + a) 0)
    ]

testWeighted' :: TestTree
testWeighted' =
  testGroup
    "weighted'"
    [ testProperty "weighted' f a q == weighted f a q" $ do
        forAll randomWeightedAcceptors $ \acceptors ->
          forAll (subsetOf acceptors) $ \maybeQuorum ->
            let isQuorum = weighted snd acceptors maybeQuorum
             in classify isQuorum "quorum" $
                  weighted' snd acceptors maybeQuorum === isQuorum
    ]

subsetOf :: Ord a => Set a -> Gen (Set a)
subsetOf = foldrM addSome mempty
  where
    addSome a s = (\b -> if b then s else Set.insert a s) <$> arbitrary

testCases :: Show (f a) => (f a -> f a -> Bool) -> [(f a, f a, Bool)] -> [TestTree]
testCases isQuorum = map $ \(acceptors, maybeQuorum, expected) ->
  let msg = if expected then " is a quorum in " else " is not a quorum in "
   in testCase (show maybeQuorum <> msg <> show acceptors) $
        isQuorum acceptors maybeQuorum @?= expected

properties :: (Show a, Ord a) => Gen (Set a) -> (Set a -> Set a -> Bool) -> (Set a -> Word) -> [TestTree]
properties randomAcceptors isQuorum weightOf =
  [ testProperty "prop_quorumAssumption1" $
      forAll randomAcceptors $ \acceptors ->
        forAll (quorumOf acceptors) (prop_quorumAssumption1 acceptors),
    testProperty "prop_quorumAssumption2" $
      forAll randomAcceptors $ \acceptors ->
        forAll ((,) <$> quorumOf acceptors <*> quorumOf acceptors) (uncurry prop_quorumAssumption2),
    testProperty "prop_quorumNonEmpty" $
      forAll randomAcceptors $ \acceptors ->
        forAll (quorumOf acceptors) prop_quorumNonEmpty,
    testProperty "complement is (non) quorum, unless split vote" $
      forAll randomAcceptors $ \acceptors ->
        forAll (subsetOf acceptors) $ \maybeQuorum -> do
          let complement = acceptors `Set.difference` maybeQuorum
              splitVote = weightOf maybeQuorum == weightOf complement
          classify splitVote "split vote" $
            if splitVote
              then isQuorum acceptors maybeQuorum === False .&&. isQuorum acceptors complement === False
              else isQuorum acceptors maybeQuorum =/= isQuorum acceptors complement
  ]
  where
    quorumOf acceptors = subsetOf acceptors `suchThat` isQuorum acceptors

isNotEmpty :: Foldable f => f a -> Bool
isNotEmpty = not . null
