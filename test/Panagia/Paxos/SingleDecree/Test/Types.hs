{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Panagia.Paxos.SingleDecree.Test.Types
  ( Ballot (..),
    ballot0,
    succBallot,
    Role (..),
    Node (..),
    NodeState (..),
    Message (..),
    AcceptorPersistentState (..),
    acceptorMaxBallot,
    acceptorVote,
    ProposerPersistentState (..),
    proposerBallot,
    State (..),
    proposers,
    acceptors,
    learners,
    messages,
    persistentState,
    volatileState,
    Config (..),
    isQuorum,
    configAcceptors,
    configLearners,
  )
where

import Control.Lens (Lens', lens)
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..), defaultCompare, defaultEq, defaultGeq, defaultNeq)
import Data.GADT.Show (GShow (..), defaultGshowsPrec)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Some (Some)
import Data.String (IsString (..))
import Data.Word (Word64)
import qualified Panagia.Paxos.SingleDecree as S

data Role = Proposer | Acceptor | Learner

data Node (r :: Role) where
  ProposerNode :: String -> Node Proposer
  AcceptorNode :: String -> Node Acceptor
  LearnerNode :: String -> Node Learner

deriving instance Show (Node r)

deriving instance Eq (Node r)

deriving instance Ord (Node r)

instance IsString (Node Proposer) where fromString = ProposerNode

instance IsString (Node Acceptor) where fromString = AcceptorNode

instance IsString (Node Learner) where fromString = LearnerNode

data Ballot = Ballot Word64 (Node Proposer)
  deriving (Show, Eq, Ord)

ballot0 :: Node Proposer -> Ballot
ballot0 = Ballot 0

succBallot :: Ballot -> Ballot
succBallot (Ballot i n) = Ballot (succ i) n

data Message value to where
  Prepare :: Node Proposer -> Node Acceptor -> S.Prepare Ballot -> Message value Acceptor
  Promise :: Node Acceptor -> Node Proposer -> S.Promise Ballot value -> Message value Proposer
  Accept :: Node Proposer -> Node Acceptor -> S.Accept Ballot value -> Message value Acceptor
  Accepted :: Node Acceptor -> Node Learner -> S.Accepted Ballot value -> Message value Learner

deriving instance (Show value) => Show (Message value to)

instance (Ord value) => Eq (Message value to) where
  (==) = defaultEq
  (/=) = defaultNeq

instance (Ord value) => Ord (Message value to) where
  compare = defaultCompare

instance (Ord value) => GEq (Message value) where
  geq = defaultGeq

instance (Ord value) => GCompare (Message value) where
  -- Prepare < Promise < Accept < Accepted
  gcompare a b = case a of
    Prepare f t m -> case b of
      Prepare f' t' m' -> case compare (f, t, m) (f', t', m') of
        LT -> GLT
        EQ -> GEQ
        GT -> GGT
      Promise {} -> GLT
      Accept {} -> GLT
      Accepted {} -> GLT
    Promise f t m -> case b of
      Prepare {} -> GGT
      Promise f' t' m' -> case compare (f, t, m) (f', t', m') of
        LT -> GLT
        EQ -> GEQ
        GT -> GGT
      Accept {} -> GLT
      Accepted {} -> GLT
    Accept f t m -> case b of
      Prepare {} -> GGT
      Promise {} -> GGT
      Accept f' t' m' -> case compare (f, t, m) (f', t', m') of
        LT -> GLT
        EQ -> GEQ
        GT -> GGT
      Accepted {} -> GLT
    Accepted f t m -> case b of
      Prepare {} -> GGT
      Promise {} -> GGT
      Accept {} -> GGT
      Accepted f' t' m' -> case compare (f, t, m) (f', t', m') of
        LT -> GLT
        EQ -> GEQ
        GT -> GGT

instance (Show value) => GShow (Message value) where
  gshowsPrec = defaultGshowsPrec

{- HLINT ignore ProposerPersistentState "Use newtype instead of data" -}
data ProposerPersistentState = ProposerPersistentState
  { _proposerBallot :: Maybe Ballot
  }
  deriving (Show, Eq)

proposerBallot :: Lens' ProposerPersistentState (Maybe Ballot)
proposerBallot = lens _proposerBallot (\st b -> st {_proposerBallot = b})

type ProposerVolatileState value = S.ProposerState (Node Acceptor) Ballot value

data AcceptorPersistentState value = AcceptorPersistentState
  { _acceptorMaxBallot :: Maybe Ballot,
    _acceptorVote :: Maybe (S.Vote Ballot value)
  }
  deriving (Show, Eq)

acceptorMaxBallot :: Lens' (AcceptorPersistentState value) (Maybe Ballot)
acceptorMaxBallot = lens _acceptorMaxBallot (\st b -> st {_acceptorMaxBallot = b})

acceptorVote :: Lens' (AcceptorPersistentState value) (Maybe (S.Vote Ballot value))
acceptorVote = lens _acceptorVote (\st v -> st {_acceptorVote = v})

type LearnerVolatileState value = S.LearnerState (Node Acceptor) Ballot value

data NodeState value r where
  ProposerState ::
    { _proposerPersistentState :: ProposerPersistentState,
      _proposerVolatileState :: ProposerVolatileState value
    } ->
    NodeState value Proposer
  AcceptorState ::
    { _acceptorPersistentState :: AcceptorPersistentState value
    } ->
    NodeState value Acceptor
  LearnerState ::
    { _learnerVolatileState :: Either value (LearnerVolatileState value)
    } ->
    NodeState value Learner

deriving instance (Show value) => Show (NodeState value r)

deriving instance (Ord value) => Eq (NodeState value r)

class HasVolatileState s p | s -> p where
  volatileState :: Lens' s p

instance HasVolatileState (NodeState value Proposer) (ProposerVolatileState value) where
  volatileState = lens _proposerVolatileState (\st v -> st {_proposerVolatileState = v})

instance HasVolatileState (NodeState value Learner) (Either value (LearnerVolatileState value)) where
  volatileState = lens _learnerVolatileState (\st v -> st {_learnerVolatileState = v})

class HasPersistentState s p | s -> p where
  persistentState :: Lens' s p

instance HasPersistentState (NodeState value Proposer) ProposerPersistentState where
  persistentState = lens _proposerPersistentState (\st p -> st {_proposerPersistentState = p})

instance HasPersistentState (NodeState value Acceptor) (AcceptorPersistentState value) where
  persistentState = lens _acceptorPersistentState (\st p -> st {_acceptorPersistentState = p})

data State value = State
  { _proposers :: Map (Node Proposer) (NodeState value Proposer),
    _acceptors :: Map (Node Acceptor) (NodeState value Acceptor),
    _learners :: Map (Node Learner) (NodeState value Learner),
    _messages :: Set (Some (Message value))
  }
  deriving (Show)

proposers :: Lens' (State value) (Map (Node Proposer) (NodeState value Proposer))
proposers = lens _proposers (\st p -> st {_proposers = p})

acceptors :: Lens' (State value) (Map (Node Acceptor) (NodeState value Acceptor))
acceptors = lens _acceptors (\st p -> st {_acceptors = p})

learners :: Lens' (State value) (Map (Node Learner) (NodeState value Learner))
learners = lens _learners (\st l -> st {_learners = l})

messages :: Lens' (State value) (Set (Some (Message value)))
messages = lens _messages (\st m -> st {_messages = m})

instance (Ord value) => Semigroup (State value) where
  State pa aa la ma <> State pb ab lb mb = State (pa <> pb) (aa <> ab) (la <> lb) (ma <> mb)

instance (Ord value) => Monoid (State value) where
  mempty = State mempty mempty mempty mempty

data Config = Config
  { _configAcceptors :: Set (Node Acceptor),
    _configLearners :: Set (Node Learner),
    _configIsQuorum :: Set (Node Acceptor) -> Bool
  }

configAcceptors :: Lens' Config (Set (Node Acceptor))
configAcceptors = lens _configAcceptors (\st s -> st {_configAcceptors = s})

configLearners :: Lens' Config (Set (Node Learner))
configLearners = lens _configLearners (\st s -> st {_configLearners = s})

isQuorum :: Lens' Config (Set (Node Acceptor) -> Bool)
isQuorum = lens _configIsQuorum (\st q -> st {_configIsQuorum = q})
