{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Panagia.Paxos.Pure
  ( runPaxos,
    ballot0,
    Command (..),
    Value (..),
    Config (..),
    NodeId (..),
    State (..),
  )
where

import Control.Lens.Getter (Contravariant, to, view)
import Control.Lens.Type (Optic')
import Control.Monad.Trans.Free (iterTM)
import Control.Monad.Trans.RWS (gets, modify, runRWST, tell)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Profunctor (Profunctor)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Natural (Natural)
import Panagia.Paxos (Message, MessageType (..), ProposerState, Role (..))
import Panagia.Paxos.Free (Paxos, PaxosT, runPaxosT)
import qualified Panagia.Paxos.Free as P

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

data State (r :: Role) where
  StateProposer :: ProposerState (PaxosT Ballot NodeId Value Identity) -> State Proposer
  StateAcceptor :: Maybe Ballot -> Maybe (Ballot, Value) -> State Acceptor

data Command
  = BroadcastPropose NodeId (Message (PaxosT Ballot NodeId Value Identity) Propose)
  | UnicastPromise NodeId NodeId (Message (PaxosT Ballot NodeId Value Identity) Promise)
  | BroadcastAccept NodeId (Message (PaxosT Ballot NodeId Value Identity) Accept)
  | UnicastAccepted NodeId NodeId (Message (PaxosT Ballot NodeId Value Identity) Accepted)

runPaxos :: Paxos Ballot NodeId Value t a -> Config -> State t -> (a, State t, [Command])
runPaxos act cfg state = runIdentity $ runRWST act' cfg state
  where
    act' = flip iterTM (runPaxosT act) $ \case
      P.BroadcastPropose msg cont -> do
        node <- view configNodeId
        tell [BroadcastPropose node msg]
        cont
      P.UnicastPromise n msg cont -> do
        node <- view configNodeId
        tell [UnicastPromise node n msg]
        cont
      P.BroadcastAccept msg cont -> do
        node <- view configNodeId
        tell [BroadcastAccept node msg]
        cont
      P.UnicastAccepted n msg cont -> do
        node <- view configNodeId
        tell [UnicastAccepted node n msg]
        cont
      P.NewBallot (Ballot idx _) cont -> do
        node <- view configNodeId
        cont (Ballot (succ idx) node)
      P.QuorumReached votes cont -> do
        c <- Set.size <$> view configNodes
        let q = (c `div` 2) + 1
        cont $ Set.size votes >= q
      P.StoreBallot b cont -> do
        modify $ \case
          StateProposer {} -> error "Impossible"
          StateAcceptor _ p -> StateAcceptor (Just b) p
        cont
      P.GetBallot cont -> do
        b <- gets $ \case
          StateProposer {} -> error "Impossible"
          StateAcceptor b _ -> b
        cont b
      P.StoreProposal p cont -> do
        modify $ \case
          StateProposer {} -> error "Impossible"
          StateAcceptor b _ -> StateAcceptor b (Just p)
        cont
      P.GetProposal cont -> do
        p <- gets $ \case
          StateProposer {} -> error "Impossible"
          StateAcceptor _ p -> p
        cont p
      P.GetProposerState cont -> do
        p <- gets $ \case
          StateAcceptor {} -> error "Impossible"
          StateProposer p -> p
        cont p
      P.SetProposerState p cont -> do
        modify $ \case
          StateAcceptor {} -> error "Impossible"
          StateProposer {} -> StateProposer p
        cont
