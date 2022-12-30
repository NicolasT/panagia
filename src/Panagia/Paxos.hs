{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Panagia.Paxos
  ( MonadPaxos (..),
    MessageType (..),
    Role (..),
    HasBallot (..),
    MayHaveProposal (..),
    HasValue (..),
    HasProposal (..),
    ProposerState,
    initProposerState,
    phase1a,
    phase1b,
    handlePromise,
    phase2b,
    handleAccepted,
    prop_newBallot,
    prop_phase1b,
    prop_newBallot',
  )
where

import Control.Lens.At (at)
import Control.Lens.Getter (Contravariant, Getter, to, use, view)
import Control.Lens.Lens (Lens', lens, (<%=))
import Control.Lens.Setter ((.=), (?=))
import Control.Lens.Type (Optic')
import Control.Monad (when)
import Control.Monad.Logger (LogLevel (..), LogStr, MonadLogger, ToLogStr (..), logWithoutLoc)
import Control.Monad.State.Class (MonadState)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Ord (Down (..))
import Data.Profunctor (Profunctor)
import Data.Set (Set)
import qualified Data.Set as Set

-- The kind of messages used in the protocol
data MessageType = Propose | Promise | Accept | Accepted

-- The role of a node
data Role = Proposer | Acceptor

-- For a given message kind, the role of a node able to send it
type family Sender (m :: MessageType) :: Role where
  Sender Propose = Proposer
  Sender Promise = Acceptor
  Sender Accept = Proposer
  Sender Accepted = Acceptor

-- Role of a node's peer, i.e., the inverse role
type family Peer (r :: Role) = (r' :: Role) | r' -> r where
  Peer Proposer = Acceptor
  Peer Acceptor = Proposer

-- For a given message kind, the role of a node able to receive it.
-- This is, essentially, the peer role of the sender of m.
type Receiver m = Peer (Sender m)

-- A proposal combines a value with the ballot at which it was accepted.
type Proposal b v = (b, v)

-- A message with a ballot inside
class HasBallot m b | m -> b where
  ballot :: Getter m b

-- A message which may have a proposal inside
class MayHaveProposal m b v | m -> b, m -> v where
  maybeProposal :: Getter m (Maybe (Proposal b v))

-- A message with a value inside
class HasValue m v | m -> v where
  value :: Getter m v

-- A message with a proposal inside
class HasProposal m b v | m -> b, m -> v where
  proposal :: Getter m (Proposal b v)

-- A propose message carries a ballot
type ProposeMessage m b = (HasBallot m b)

-- A promise message carries a ballot and maybe a proposal
type PromiseMessage m b v = (HasBallot m b, MayHaveProposal m b v)

-- An accept message carries a ballot and a value
type AcceptMessage m b v = (HasBallot m b, HasValue m v)

-- An accepted message carries a proposal
type AcceptedMessage m b v = (HasProposal m b v)

-- Kind of message communication type
data CommunicationType = Broadcast | Unicast

-- Mapping of message type to communication type
type family DeliveryType (m :: MessageType) :: CommunicationType where
  DeliveryType Propose = Broadcast
  DeliveryType Promise = Unicast
  DeliveryType Accept = Broadcast
  DeliveryType Accepted = Unicast

class
  ( forall r. Monad (m r),
    Ord (Ballot m),
    Ord (NodeId m),
    ProposeMessage (Message m Propose) (Ballot m),
    PromiseMessage (Message m Promise) (Ballot m) (Value m),
    AcceptMessage (Message m Accept) (Ballot m) (Value m),
    AcceptedMessage (Message m Accepted) (Ballot m) (Value m)
  ) =>
  MonadPaxos (m :: Role -> Type -> Type)
  where
  data Message m :: MessageType -> Type
  type Ballot m :: Type
  type Value m :: Type
  type NodeId m :: Type

  propose :: Ballot m -> Message m Propose
  promise :: Ballot m -> Maybe (Proposal (Ballot m) (Value m)) -> Message m Promise
  accept :: Ballot m -> Value m -> Message m Accept
  accepted :: Proposal (Ballot m) (Value m) -> Message m Accepted

  broadcast :: DeliveryType t ~ Broadcast => Message m t -> m (Sender t) ()
  unicast :: DeliveryType t ~ Unicast => NodeId m -> Message m t -> m (Sender t) ()

  newBallot :: Ballot m -> m Proposer (Ballot m)

  quorumReached :: Set (NodeId m) -> m Proposer Bool

  storeBallot :: Ballot m -> m Acceptor ()
  getBallot :: m Acceptor (Maybe (Ballot m))
  storeProposal :: Proposal (Ballot m) (Value m) -> m Acceptor ()
  getProposal :: m Acceptor (Maybe (Proposal (Ballot m) (Value m)))

prop_newBallot :: MonadPaxos m => Ballot m -> m Proposer Bool
prop_newBallot b = do
  next <- newBallot b
  return (next > b)

type MessageHandler m t = MonadPaxos m => NodeId m -> Message m t -> m (Receiver t) ()

data ProposerState m = ProposerState
  { _proposerStateValue :: Value m,
    _proposerStateBallot :: Ballot m,
    _proposerStatePromises :: Map (NodeId m) (Maybe (Proposal (Ballot m) (Value m))),
    _proposerStateAccepteds :: Map (NodeId m) (Proposal (Ballot m) (Value m))
  }

deriving instance (Show (Value m), Show (NodeId m), Show (Ballot m)) => Show (ProposerState m)

deriving instance (Eq (Value m), Eq (NodeId m), Eq (Ballot m)) => Eq (ProposerState m)

initProposerState :: MonadPaxos m => Ballot m -> Value m -> ProposerState m
initProposerState ballot0 v =
  ProposerState
    { _proposerStateValue = v,
      _proposerStateBallot = ballot0,
      _proposerStatePromises = mempty,
      _proposerStateAccepteds = mempty
    }

proposerStateValue :: (Profunctor p, Contravariant f) => Optic' p f (ProposerState m) (Value m)
proposerStateValue = to _proposerStateValue

proposerStateBallot :: Lens' (ProposerState m) (Ballot m)
proposerStateBallot = lens _proposerStateBallot (\s b -> s {_proposerStateBallot = b})

proposerStatePromises :: Lens' (ProposerState m) (Map (NodeId m) (Maybe (Proposal (Ballot m) (Value m))))
proposerStatePromises = lens _proposerStatePromises (\s p -> s {_proposerStatePromises = p})

proposerStateAccepteds :: Lens' (ProposerState m) (Map (NodeId m) (Proposal (Ballot m) (Value m)))
proposerStateAccepteds = lens _proposerStateAccepteds (\s a -> s {_proposerStateAccepteds = a})

phase1a :: (MonadPaxos m, MonadLogger (m Proposer), ToLogStr (Ballot m), MonadState (ProposerState m) (m Proposer)) => m Proposer ()
phase1a = do
  newBallot'
  b <- use proposerStateBallot
  logWithoutLoc "Panagia.Paxos.phase1a" LevelDebug $ "Broadcasting Propose for ballot " <> toLogStr b
  broadcast $ propose b

newBallot' :: (MonadPaxos m, MonadLogger (m Proposer), ToLogStr (Ballot m), MonadState (ProposerState m) (m Proposer)) => m Proposer ()
newBallot' = do
  logWithoutLoc "Panagia.Paxos.phase1a" LevelInfo ("Progressing to next ballot" :: LogStr)
  currentBallot <- use proposerStateBallot
  logWithoutLoc "Panagia.Paxos.phase1a" LevelDebug $ "Current ballot is " <> toLogStr currentBallot
  nextBallot <- newBallot currentBallot
  logWithoutLoc "Panagia.Paxos.phase1a" LevelDebug $ "New ballot is " <> toLogStr nextBallot
  proposerStateBallot .= nextBallot
  -- New ballot, empty vote tracking maps
  proposerStatePromises .= mempty
  proposerStateAccepteds .= mempty

prop_newBallot' :: (MonadPaxos m, MonadLogger (m Proposer), ToLogStr (Ballot m), MonadState (ProposerState m) (m Proposer)) => m Proposer Bool
prop_newBallot' = do
  firstBallot <- use proposerStateBallot

  newBallot'

  lastBallot <- use proposerStateBallot
  promises <- use proposerStatePromises
  accepteds <- use proposerStateAccepteds

  return $
    lastBallot > firstBallot
      && Map.null promises
      && Map.null accepteds

phase1b :: MessageHandler m Propose
phase1b sender msg = do
  maxBal <- getBallot
  let bal = view ballot msg

  when (maybe True (bal >) maxBal) $ do
    storeBallot bal
    currentProposal <- getProposal
    unicast sender $ promise bal currentProposal

prop_phase1b :: MonadPaxos m => NodeId m -> Message m Propose -> m Acceptor Bool
prop_phase1b sender msg = do
  fstBallot <- getBallot
  phase1b sender msg
  sndBallot <- getBallot
  let msgBallot = view ballot msg

  return $
    if Just msgBallot > fstBallot
      then sndBallot == Just msgBallot
      else sndBallot == fstBallot

handlePromise :: (MonadState (ProposerState m) (m Proposer), MonadLogger (m Proposer), ToLogStr (Ballot m)) => MessageHandler m Promise
handlePromise sender msg = do
  currentBallot <- use proposerStateBallot
  case compare currentBallot (view ballot msg) of
    LT -> do
      proposerStateBallot .= view ballot msg
      phase1a
    GT -> return ()
    EQ -> do
      proposerStatePromises . at sender ?= view maybeProposal msg
      whenM quorumReached' phase2a
  where
    quorumReached' = do
      promises <- use proposerStatePromises
      quorumReached (Set.fromList (Map.keys promises))

phase2a :: (MonadPaxos m, MonadState (ProposerState m) (m Proposer)) => m Proposer ()
phase2a = do
  ps <- use proposerStatePromises
  v <- maybe (use proposerStateValue) return $ case sortOn (\(b, _) -> Down b) (catMaybes (Map.elems ps)) of
    [] -> Nothing
    ((_, v) : _) -> Just v

  currentBallot <- use proposerStateBallot
  broadcast $ accept currentBallot v

phase2b :: MessageHandler m Accept
phase2b sender msg = do
  maxBal <- getBallot
  let bal = view ballot msg

  when (maybe True (bal >=) maxBal) $ do
    storeBallot bal
    let p = (bal, view value msg)
    storeProposal p
    unicast sender $ accepted p

handleAccepted :: (MonadState (ProposerState m) (m Proposer), MonadLogger (m Proposer), ToLogStr (Ballot m)) => MessageHandler m Accepted
handleAccepted sender msg = do
  currentBallot <- use proposerStateBallot
  let (b, _) = view proposal msg
  case compare currentBallot b of
    LT -> do
      proposerStateBallot .= b
      phase1a
    GT -> return ()
    EQ -> do
      as <- proposerStateAccepteds <%= Map.insert sender (view proposal msg)
      whenM (quorumReached (Set.fromList (Map.keys as))) $ do
        return ()

whenM :: Monad m => m Bool -> m () -> m ()
whenM check act = check >>= flip when act
