{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Panagia.Paxos.Pure
  ( runPaxos,
    ballot0,
    Value (..),
    Command (..),
    Config (..),
    NodeId (..),
  )
where

import Control.Lens.Getter (Contravariant, to, view)
import Control.Lens.Type (Optic')
import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState (..), gets, modify)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Data.Kind (Type)
import Data.Profunctor (Profunctor)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Natural (Natural)
import qualified Panagia.Paxos as P

data Command
  = BroadcastPropose NodeId (P.Message Paxos P.Propose)
  | UnicastPromise NodeId NodeId (P.Message Paxos P.Promise)
  | BroadcastAccept NodeId (P.Message Paxos P.Accept)
  | UnicastAccepted NodeId NodeId (P.Message Paxos P.Accepted)
  deriving (Show, Eq)

type family State (r :: P.Role) :: Type where
  State P.Proposer = P.ProposerState Paxos
  State P.Acceptor = (Maybe Ballot, Maybe (Ballot, Value))

newtype Paxos r a = Paxos {unPaxos :: RWS Config [Command] (State r) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Config, MonadWriter [Command])

runPaxos :: Paxos t a -> Config -> State t -> (a, State t, [Command])
runPaxos = runRWS . unPaxos

data Ballot = Ballot Natural NodeId
  deriving (Show, Eq, Ord)

ballot0 :: Ballot
ballot0 = Ballot 0 (NodeId "")

newtype NodeId = NodeId String
  deriving (Show, Eq, Ord)

newtype Value = Value Natural
  deriving (Show, Eq)

data Config = Config
  { _configNodeId :: NodeId,
    _configNodes :: Set NodeId
  }

configNodeId :: (Contravariant f, Profunctor p) => Optic' p f Config NodeId
configNodeId = to _configNodeId

configNodes :: (Profunctor p, Contravariant f) => Optic' p f Config (Set NodeId)
configNodes = to _configNodes

instance P.MonadPaxos Paxos where
  type Ballot Paxos = Ballot
  type Value Paxos = Value
  type NodeId Paxos = NodeId

  data Message Paxos t where
    Propose :: Ballot -> P.Message Paxos P.Propose
    Promise :: Ballot -> Maybe (Ballot, Value) -> P.Message Paxos P.Promise
    Accept :: Ballot -> Value -> P.Message Paxos P.Accept
    Accepted :: (Ballot, Value) -> P.Message Paxos P.Accepted

  propose = Propose
  promise = Promise
  accept = Accept
  accepted = Accepted

  broadcast msg = do
    n <- view configNodeId
    Paxos $ do
      tell $ case msg of
        Propose {} -> [BroadcastPropose n msg]
        Accept {} -> [BroadcastAccept n msg]
  unicast node msg = do
    n <- view configNodeId
    Paxos $ do
      tell $ case msg of
        Promise {} -> [UnicastPromise n node msg]
        Accepted {} -> [UnicastAccepted n node msg]

  newBallot (Ballot idx _) = do
    n <- view configNodeId
    return $ Ballot (succ idx) n

  quorumReached nodes = do
    c <- Set.size <$> view configNodes
    let q = (c `div` 2) + 1
    return $ Set.size nodes >= q

  storeBallot b = Paxos $ modify (\(_, p) -> (Just b, p))
  getBallot = Paxos $ gets fst
  storeProposal p = Paxos $ modify (\(b, _) -> (b, Just p))
  getProposal = Paxos $ gets snd

instance MonadState (P.ProposerState Paxos) (Paxos P.Proposer) where
  get = Paxos get
  put = Paxos . put

deriving instance Show (P.Message Paxos t)

deriving instance Eq (P.Message Paxos t)

instance P.HasBallot (P.Message Paxos P.Propose) Ballot where
  ballot = to $ \case
    Propose b -> b

instance P.HasBallot (P.Message Paxos P.Promise) Ballot where
  ballot = to $ \case
    Promise b _ -> b

instance P.MayHaveProposal (P.Message Paxos P.Promise) Ballot Value where
  maybeProposal = to $ \case
    Promise _ v -> v

instance P.HasBallot (P.Message Paxos P.Accept) Ballot where
  ballot = to $ \case
    Accept b _ -> b

instance P.HasValue (P.Message Paxos P.Accept) Value where
  value = to $ \case
    Accept _ v -> v

instance P.HasProposal (P.Message Paxos P.Accepted) Ballot Value where
  proposal = to $ \case
    Accepted p -> p
