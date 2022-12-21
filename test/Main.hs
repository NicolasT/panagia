{-# LANGUAGE LambdaCase #-}

module Main (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Panagia.Paxos (handleAccepted, handlePromise, initProposerState, phase1a, phase1b, phase2b)
import Panagia.Paxos.Pure (Command (..), Config (..), NodeId (..), Value (..), runPaxos)
import Test.Hspec (describe, hspec, it, shouldBe, shouldReturn)

main :: IO ()
main = hspec $ do
  describe "Simple test-case" $ do
    it "works" simpleTestCase

simpleTestCase :: IO ()
simpleTestCase = do
  let ((), ps, cs) = runPaxos phase1a proposerConfig (initProposerState value)
  loop ps nodes0 cs
    `shouldReturn` Map.fromList
      [ (acceptor0, Just value),
        (acceptor1, Just value),
        (acceptor2, Just value)
      ]
  where
    value = Value 1
    proposerConfig = Config proposer acceptors

    proposer = NodeId "proposer"
    acceptor0 = NodeId "acceptor0"
    acceptor1 = NodeId "acceptor1"
    acceptor2 = NodeId "acceptor2"

    acceptors = Set.fromList [acceptor0, acceptor1, acceptor2]

    nodes0 = Map.fromList (map (\a -> (a, (Config a acceptors, (Nothing, Nothing)))) (Set.toList acceptors))

    broadcast ps nodes cs act = do
      let (cs', nodes') =
            Map.mapAccum
              (\a (c, s) -> let ((), s', a') = runPaxos act c s in (a <> a', (c, s')))
              []
              nodes
      loop ps nodes' (cs <> cs')

    unicast ps nodes cs act = do
      let ((), ps', cs') = runPaxos act proposerConfig ps
      loop ps' nodes (cs <> cs')

    loop ps nodes = \case
      [] -> return $ Map.map (\(_, (_, v)) -> fmap snd v) nodes
      (c : cs) -> case c of
        BroadcastPropose from msg -> do
          from `shouldBe` proposer
          broadcast ps nodes cs (phase1b from msg)
        UnicastPromise from to msg -> do
          to `shouldBe` proposer
          unicast ps nodes cs (handlePromise from msg)
        BroadcastAccept from msg -> do
          from `shouldBe` proposer
          broadcast ps nodes cs (phase2b from msg)
        UnicastAccepted from to msg -> do
          to `shouldBe` proposer
          unicast ps nodes cs (handleAccepted from msg)
