{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module: Panagia.Paxos.SingleDecree
-- Description: Model-only implementation of Single-Decree Paxos
-- Copyright: (c) Nicolas Trangez, 2023
-- License: GPL-3.0-only
-- Maintainer: ikke@nicolast.be
-- Stability: alpha
--
-- This module provides a model-only implementation of Single-Decree Paxos,
-- i.e., a protocol to reach consensus over a single value across a group of
-- nodes.
--
-- In this context, "model-only" means there's no code which, e.g., performs
-- IO, or types which dictate the format of messages transmitted over the
-- network. All code is abstracted over, e.g., the type of ballots, or the type
-- of a value to reach consensus over. Only the minimal functionality required
-- of said types is expressed through type-class constraints, e.g., 'Ord' on
-- ballots.
--
-- It's roughly based on the TLA+ specification (and according TLAPS proof)
-- found at
-- https://raw.githubusercontent.com/tlaplus/tlapm/581ebd8afbe927b2b7d31c62ccec3a3409d9d8ce/examples/paxos/Paxos.tla
module Panagia.Paxos.SingleDecree
  ( -- * Running a Proposer
    MonadProposer (..),

    -- ** Laws
    monadProposerLaws,

    -- ** Event handlers

    -- | The following functions can be used to launch a new Proposer round,
    -- or handle incoming messages.
    propose,
    handlePromise,
    ProposerState,

    -- * Running an Acceptor
    MonadAcceptor (..),
    MonadAcceptorTransaction (..),

    -- ** Laws
    monadAcceptorLaws,

    -- ** Event handlers

    -- | The following functions can be used to handle incoming messages.
    handlePrepare,
    handleAccept,

    -- * Types and type aliases

    -- ** Messages exchanged between nodes in the cluster

    -- | In this module, various types of messages are represented as simple
    -- tuples. Higher-level code can pick them into separate values and embed
    -- these in larger structures, e.g., for submission through a network.

    -- *** From Proposers to Acceptors
    Prepare,
    Accept,

    -- *** From Acceptors to Proposers
    Promise,

    -- *** From Acceptors to Learners
    Accepted,

    -- ** Miscellaneous
    Vote,
    InvariantViolation (..),
  )
where

import Control.Monad (forM_, unless)
import Control.Monad.Catch (MonadThrow)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Panagia.Paxos.SingleDecree.Monad
  ( MonadAcceptor (..),
    MonadAcceptorTransaction (..),
    MonadProposer (..),
    monadAcceptorLaws,
    monadProposerLaws,
  )
import Panagia.Paxos.SingleDecree.Types
  ( Accept,
    Accepted,
    InvariantViolation (..),
    Prepare,
    Promise,
    Vote,
    invariantViolation,
  )

-- $setup
-- >>> import Test.QuickCheck

-- Proposer {{{

-- | Internal state of a Proposer node.

--
-- This is opaque to a program using this code, since the state is ephemeral
-- and only to be used across Proposer message handler invocations.
data ProposerState acceptor ballot value
  = -- | Proposer is waiting to receive 'Promise's from a quorum of nodes.
    ProposerWaitingForPromises
      !value
      -- ^ Value to propose, if none has been chosen before
      !ballot
      -- ^ Ballot generated when the proposal round started
      !(Map acceptor (Maybe (Vote ballot value)))
      -- ^ Votes received from nodes for the current ballot
  | -- | Proposer received 'Promise's from a quorum of nodes before, and
    -- hence broadcasted an 'Accept'.
    ProposerSentAccept
  deriving (Show, Eq)

-- (***************************************************************************)
-- (* Phase 1a: A leader selects a ballot number b and sends a 1a message     *)
-- (* with ballot b to a majority of acceptors.  It can do this only if it    *)
-- (* has not already sent a 1a message for ballot b.                         *)
-- (***************************************************************************)
-- Phase1a(b) == /\ ~ \E m \in msgs : (m.type = "1a") /\ (m.bal = b)
--               /\ Send([type |-> "1a", bal |-> b])
--               /\ UNCHANGED <<maxVBal, maxBal, maxVal>>

-- | Launch a new Proposer round, attempting to reach consensus for a given
-- value.
propose ::
  (MonadProposer m, Ord (ProposerAcceptorNode m)) =>
  -- | Value to propose, if none was chosen before.
  ProposerValue m ->
  -- | State of the Proposer, to be passed into a future 'handlePromise' call.
  m (ProposerState (ProposerAcceptorNode m) (ProposerBallot m) (ProposerValue m))
propose v = do
  b <- newBallot
  broadcastPrepare b
  return $ ProposerWaitingForPromises v b mempty

-- (***************************************************************************)
-- (* Phase 2a: If the leader receives a response to its 1b message (for      *)
-- (* ballot b) from a quorum of acceptors, then it sends a 2a message to all *)
-- (* acceptors for a proposal in ballot b with a value v, where v is the     *)
-- (* value of the highest-numbered proposal among the responses, or is any   *)
-- (* value if the responses reported no proposals.  The leader can send only *)
-- (* one 2a message for any ballot.                                          *)
-- (***************************************************************************)
-- Phase2a(b) ==
--   /\ ~ \E m \in msgs : (m.type = "2a") /\ (m.bal = b)
--   /\ \E v \in Values :
--        /\ \E Q \in Quorums :
--             \E S \in SUBSET {m \in msgs : (m.type = "1b") /\ (m.bal = b)} :
--                /\ \A a \in Q : \E m \in S : m.acc = a
--                /\ \/ \A m \in S : m.maxVBal = -1
--                   \/ \E c \in 0..(b-1) :
--                         /\ \A m \in S : m.maxVBal =< c
--                         /\ \E m \in S : /\ m.maxVBal = c
--                                         /\ m.maxVal = v
--        /\ Send([type |-> "2a", bal |-> b, val |-> v])
--   /\ UNCHANGED <<maxBal, maxVBal, maxVal>>

-- | Handle an incoming 'Promise' message.
--
-- This can throw an 'InvariantViolation' error.
handlePromise ::
  (MonadProposer m, MonadThrow m, Ord (ProposerBallot m), Ord (ProposerAcceptorNode m)) =>
  -- | Current state of the Proposer.
  ProposerState (ProposerAcceptorNode m) (ProposerBallot m) (ProposerValue m) ->
  -- | Node from which the message was received.
  ProposerAcceptorNode m ->
  -- | The 'Promise' to handle.
  Promise (ProposerBallot m) (ProposerValue m) ->
  -- | Updated state of the Proposer, to be used in future 'handlePromise'
  -- calls.
  m (ProposerState (ProposerAcceptorNode m) (ProposerBallot m) (ProposerValue m))
handlePromise state0 sender (promiseBallot, promiseVote) = case state0 of
  ProposerSentAccept -> return ProposerSentAccept
  ProposerWaitingForPromises value ballot votes0 -> case compare promiseBallot ballot of
    LT -> return $ ProposerWaitingForPromises value ballot votes0
    GT -> invariantViolation "Received promise with higher ballot than local state"
    EQ -> do
      let votes = Map.insert sender promiseVote votes0

      isQuorum' <- isQuorum (Map.keysSet votes)
      if isQuorum'
        then do
          let v = maybe value snd (maximumBy (fmap fst) Nothing votes)
          broadcastAccept (ballot, v)
          return ProposerSentAccept
        else return $ ProposerWaitingForPromises value ballot votes

-- | Out of a structore of values, get the highest one according to some
-- mapping function, or a default value.
--
-- Some simple test-cases:
--
-- >>> maximumBy (fmap fst) Nothing []
-- Nothing
--
-- >>> maximumBy (fmap fst) Nothing [Nothing, Nothing]
-- Nothing
--
-- >>> maximumBy (fmap fst) Nothing [Nothing, Just (0, 1), Nothing]
-- Just (0,1)
--
-- >>> maximumBy (fmap fst) Nothing [Just (0, 1), Just (1, 0)]
-- Just (1,0)
--
-- prop> \((NonEmpty l) :: NonEmptyList (Maybe (Int, ()))) -> maximumBy (fmap fst) Nothing l === maximum l
maximumBy :: (Foldable t, Ord b) => (a -> b) -> a -> t a -> a
maximumBy f = foldr (\v1 v2 -> if f v1 <= f v2 then v2 else v1)

-- }}}

--- Acceptor {{{

-- (***************************************************************************)
-- (* Phase 1b: If an acceptor receives a 1a message with ballot b greater    *)
-- (* than that of any 1a message to which it has already responded, then it  *)
-- (* responds to the request with a promise not to accept any more proposals *)
-- (* for ballots numbered less than b and with the highest-numbered ballot   *)
-- (* (if any) for which it has voted for a value and the value it voted for  *)
-- (* in that ballot.  That promise is made in a 1b message.                  *)
-- (***************************************************************************)
-- Phase1b(a) ==
--   \E m \in msgs :
--      /\ m.type = "1a"
--      /\ m.bal > maxBal[a]
--      /\ Send([type |-> "1b", bal |-> m.bal, maxVBal |-> maxVBal[a],
--                maxVal |-> maxVal[a], acc |-> a])
--      /\ maxBal' = [maxBal EXCEPT ![a] = m.bal]
--      /\ UNCHANGED <<maxVBal, maxVal>>

-- | Handle an incoming 'Prepare' message.
handlePrepare ::
  (MonadAcceptor m, Ord (AcceptorBallot m)) =>
  -- | Node from which the message was received.
  AcceptorProposerNode m ->
  -- | The 'Prepare' to handle.
  Prepare (AcceptorBallot m) ->
  m ()
handlePrepare sender ballot = do
  maybePromise <- transactionally $ do
    maxBal <- getMaxBallot

    if Just ballot > maxBal
      then do
        currentVote <- getVote

        setMaxBallot ballot
        pure $ Just (ballot, currentVote)
      else return Nothing

  forM_ maybePromise (sendPromise sender)

-- (***************************************************************************)
-- (* Phase 2b: If an acceptor receives a 2a message for a ballot numbered    *)
-- (* b, it votes for the message's value in ballot b unless it has already   *)
-- (* responded to a 1a request for a ballot number greater than or equal to  *)
-- (* b.                                                                      *)
-- (***************************************************************************)
-- Phase2b(a) ==
--   \E m \in msgs :
--     /\ m.type = "2a"
--     /\ m.bal >= maxBal[a]
--     /\ Send([type |-> "2b", bal |-> m.bal, val |-> m.val, acc |-> a])
--     /\ maxVBal' = [maxVBal EXCEPT ![a] = m.bal]
--     /\ maxBal' = [maxBal EXCEPT ![a] = m.bal]
--     /\ maxVal' = [maxVal EXCEPT ![a] = m.val]

-- | Handle an incoming 'Accept' message.
handleAccept ::
  (MonadAcceptor m, Ord (AcceptorBallot m)) =>
  -- | The 'Accept' to handle.
  Accept (AcceptorBallot m) (AcceptorValue m) ->
  m ()
handleAccept (ballot, value) = do
  maybeAccepted <- transactionally $ do
    maxBal <- getMaxBallot

    if Just ballot >= maxBal
      then do
        unless (maxBal == Just ballot) $
          setMaxBallot ballot

        setVote (ballot, value)
        return $ Just (ballot, value)
      else return Nothing

  forM_ maybeAccepted broadcastAccepted

-- }}}
