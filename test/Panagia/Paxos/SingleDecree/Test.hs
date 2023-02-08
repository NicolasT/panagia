{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Panagia.Paxos.SingleDecree.Test (tests) where

import Control.Lens (at, use, (^.))
import Control.Monad (forM_, (>=>))
import Control.Monad.Catch (catch)
import Data.Foldable (foldrM)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.String (fromString)
import Hedgehog
  ( Property,
    PropertyT,
    assert,
    evalM,
    evalMaybeM,
    failure,
    forAll,
    property,
    (===),
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Panagia.Paxos.SingleDecree (InvariantViolation (..))
import Panagia.Paxos.SingleDecree.Test.Monad
  ( consensusReached,
    evalTestT,
    handleMessage,
    handleMessages,
    initProposer,
    resetLearner,
  )
import qualified Panagia.Paxos.SingleDecree.Test.Monad as M
import Panagia.Paxos.SingleDecree.Test.Types
  ( Message (Promise),
    messages,
    persistentState,
    proposerBallot,
    proposers,
    succBallot,
  )
import Panagia.Paxos.SingleDecree.Test.Utils
  ( annotating,
    dropping,
    duplicating,
    eventually,
    receiveMax,
    receiveMin,
    receiveRandom,
  )
import Panagia.Quorum (simpleMajority')
import Test.Hspec.Expectations.Lifted (expectationFailure, shouldReturn)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Test.Tasty.Hedgehog (testPropertyNamed)

simpleCase :: Assertion
simpleCase = do
  let acc = ["a1", "a2", "a3"]
      learner = "l1"
      proposer = "p1"

  evalTestT (simpleMajority' acc) acc [learner] $ do
    initProposer proposer ()
    handleMessages receiveMax

    use messages `shouldReturn` mempty

    consensusReached learner `shouldReturn` Just ()

consensusRetained :: Assertion
consensusRetained = do
  let acc = ["a1", "a2", "a3"]
      learner = "l1"
      q = simpleMajority' acc

  evalTestT q acc [learner] $ do
    let proposer1 = "p1"
        proposer2 = "p2"
        pickMessage = receiveMin

    initProposer proposer1 (1 :: Int)
    handleMessages pickMessage
    consensusReached learner `shouldReturn` Just 1

    resetLearner learner
    consensusReached learner `shouldReturn` Nothing

    initProposer proposer2 2
    handleMessages pickMessage
    consensusReached learner `shouldReturn` Just 1

    resetLearner learner
    consensusReached learner `shouldReturn` Nothing

    initProposer proposer1 2
    handleMessages pickMessage
    consensusReached learner `shouldReturn` Just 1

handlePromiseRaises :: Assertion
handlePromiseRaises = evalTestT (const False) [] [] $ do
  let p = "proposer1"
      a = "acceptor1"

  initProposer p ()
  use (proposers . at p) >>= \case
    Nothing -> expectationFailure "Proposer state not found"
    Just s' -> case s' ^. persistentState . proposerBallot of
      Nothing -> expectationFailure "Proposer ballot not set"
      Just b -> do
        let act = do
              handleMessage (Promise a p (succBallot b, Nothing))
              expectationFailure "No exception raised"

        act `catch` (\InvariantViolation {} -> return ())

threeProposers :: (forall a m. (Monad m, Ord a, Show a) => Set a -> PropertyT m (Maybe a, Set a)) -> Property
threeProposers pick = property $ do
  let acc = ["a1", "a2", "a3"]
      learner = "l1"
      pick' = pick >=> annotating

  evalTestT (simpleMajority' acc) acc [learner] $ do
    let proposer1 = "p1"
        proposer2 = "p2"
        proposer3 = "p3"

    eventually (isJust <$> consensusReached learner) $ do
      initProposer proposer1 (1 :: Int)
      initProposer proposer2 2
      initProposer proposer3 3

      handleMessages pick'

    c <- evalMaybeM (consensusReached learner)
    assert $ c `elem` ([1, 2, 3] :: [Int])

    -- Act as if this is a new learner, who hasn't received any messages yet
    -- Due to the following, it will receive Accepted messages, but the
    -- accepted value should be the one that was accepted earlier, not one
    -- of the 'new' values.
    n <- evalM $ do
      resetLearner learner
      consensusReached learner

    n === Nothing

    eventually (isJust <$> consensusReached learner) $ do
      initProposer proposer1 4
      initProposer proposer2 5
      initProposer proposer3 6
      handleMessages pick'

    c' <- evalMaybeM (consensusReached learner)
    c' === c

fullyRandom :: Property
fullyRandom = property $ do
  acceptors <- forAll $ Gen.set (Range.linear 1 15) (genNode "a")
  learners <- forAll $ Gen.set (Range.linear 1 15) (genNode "l")
  proposers' <- forAll $ Gen.map (Range.linear 1 50) genProposer

  loss <- forAll $ Gen.double $ Range.constant 0 0.20
  duplication <- forAll $ Gen.double $ Range.constant 0 0.30

  let pick = receiveRandom >=> duplicating duplication >=> dropping loss >=> annotating

  let consensusEverywhere = foldrM (\l b -> (\c -> b && isJust c) <$> consensusReached l) True learners

  evalTestT (simpleMajority' acceptors) acceptors learners $ do
    eventually consensusEverywhere $ do
      forM_ (Map.toList proposers') $ uncurry initProposer
      handleMessages pick

    cs <- foldrM (\l r -> (\c -> (l, c) : r) <$> consensusReached l) [] learners
    case cs of
      [] -> failure
      ((_, c) : _) -> case c of
        Nothing -> failure
        Just _ -> forM_ cs $ \(_, c') -> c' === c
  where
    genNode p = fromString . (p <>) <$> Gen.string (Range.constant 1 10) Gen.alphaNum
    genProposer = (,) <$> genNode "p" <*> Gen.int Range.linearBounded

tests :: TestTree
tests =
  testGroup
    "Panagia.Paxos.SingleDecree"
    [ M.tests,
      testCase "Simple case" simpleCase,
      testCase "Consensus is retained" consensusRetained,
      testCase "handlePromise raises when receiving a higher ballot" handlePromiseRaises,
      testPropertyNamed
        "Three proposers, randomized"
        "threeProposers receiveRandom"
        (threeProposers receiveRandom),
      testPropertyNamed
        "Three proposers, randomized, duplicating 10%"
        "threeProposers (receiveRandom >=> duplicating 0.10)"
        (threeProposers (receiveRandom >=> duplicating 0.10)),
      testPropertyNamed
        "Three proposers, randomized, dropping 10%"
        "threeProposers (receiveRandom >=> dropping 0.10"
        (threeProposers (receiveRandom >=> dropping 0.10)),
      testPropertyNamed
        "Three proposers, randomized, duplicating 20%, dropping 10%"
        "threeProposers (receiveRandom >=> duplicating 0.20 >=> dropping 0.10)"
        (threeProposers (receiveRandom >=> duplicating 0.20 >=> dropping 0.10)),
      testPropertyNamed
        "Fully randomized scenario"
        "fullyRandom"
        fullyRandom
    ]
