{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Panagia.Paxos.SingleDecree.Test (tests) where

import Control.Lens (at, use, (.=), (^.))
import Control.Monad ((>=>))
import Control.Monad.Catch (catch)
import Data.Maybe (isJust)
import Data.Set (Set)
import Hedgehog
  ( Property,
    PropertyT,
    assert,
    evalM,
    evalMaybeM,
    property,
    (===),
  )
import Panagia.Paxos.SingleDecree (InvariantViolation (..))
import Panagia.Paxos.SingleDecree.Test.Monad
  ( consensusReached,
    evalTestT,
    handleMessage,
    handleMessages,
    initProposer,
  )
import qualified Panagia.Paxos.SingleDecree.Test.Monad as M
import Panagia.Paxos.SingleDecree.Test.Types
  ( Message (Promise),
    learners,
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

    initProposer proposer2 2
    handleMessages pickMessage
    consensusReached learner `shouldReturn` Just 1

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
      learners . at learner .= Just mempty
      consensusReached learner

    n === Nothing

    eventually (isJust <$> consensusReached learner) $ do
      initProposer proposer1 4
      initProposer proposer2 5
      initProposer proposer3 6
      handleMessages pick'

    c' <- evalMaybeM (consensusReached learner)
    c' === c

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
        (threeProposers (receiveRandom >=> duplicating 0.20 >=> dropping 0.10))
    ]
