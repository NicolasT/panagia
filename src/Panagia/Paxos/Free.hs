{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Panagia.Paxos.Free
  ( PaxosT,
    Paxos,
    runPaxosT,
    runPaxos,
    Command (..),
    Message (..),
  )
where

import Control.Lens.Getter (to)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, MonadLogger (..), toLogStr)
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Trans.Free (Free, FreeT, liftF)
import Data.Functor.Identity (Identity)
import Data.Set (Set)
import Panagia.Paxos (HasBallot, HasProposal, HasValue, MayHaveProposal, MonadPaxos (..), Role)
import qualified Panagia.Paxos as P
import Test.QuickCheck (Arbitrary (..))

data Command ballot nodeId value m param
  = BroadcastPropose (Message (PaxosT ballot nodeId value m) P.Propose) param
  | UnicastPromise nodeId (Message (PaxosT ballot nodeId value m) P.Promise) param
  | BroadcastAccept (Message (PaxosT ballot nodeId value m) P.Accept) param
  | UnicastAccepted nodeId (Message (PaxosT ballot nodeId value m) P.Accepted) param
  | NewBallot ballot (ballot -> param)
  | QuorumReached (Set nodeId) (Bool -> param)
  | StoreBallot ballot param
  | GetBallot (Maybe ballot -> param)
  | StoreProposal (ballot, value) param
  | GetProposal (Maybe (ballot, value) -> param)
  | GetProposerState (P.ProposerState (PaxosT ballot nodeId value m) -> param)
  | SetProposerState (P.ProposerState (PaxosT ballot nodeId value m)) param
  | Log Loc LogSource LogLevel LogStr param
  deriving (Functor)

newtype PaxosT ballot nodeId value m (t :: Role) a = PaxosT {runPaxosT :: FreeT (Command ballot nodeId value m) m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail)

type Paxos ballot nodeId value = PaxosT ballot nodeId value Identity

runPaxos :: Paxos ballot nodeId value t a -> Free (Command ballot nodeId value Identity) a
runPaxos = runPaxosT

instance (Monad m, Ord ballot, Ord nodeId) => MonadPaxos (PaxosT ballot nodeId value m) where
  type Ballot (PaxosT ballot nodeId value m) = ballot
  type NodeId (PaxosT ballot nodeId value m) = nodeId
  type Value (PaxosT ballot nodeId value m) = value

  data Message (PaxosT ballot nodeId value m) t where
    Propose :: ballot -> Message (PaxosT ballot nodeId value m) P.Propose
    Promise :: ballot -> Maybe (ballot, value) -> Message (PaxosT ballot nodeId value m) P.Promise
    Accept :: ballot -> value -> Message (PaxosT ballot nodeId value m) P.Accept
    Accepted :: (ballot, value) -> Message (PaxosT ballot nodeId value m) P.Accepted

  propose = Propose
  promise = Promise
  accept = Accept
  accepted = Accepted

  broadcast msg = PaxosT $
    liftF $ case msg of
      Propose {} -> BroadcastPropose msg ()
      Accept {} -> BroadcastAccept msg ()

  unicast n msg = PaxosT $
    liftF $ case msg of
      Promise {} -> UnicastPromise n msg ()
      Accepted {} -> UnicastAccepted n msg ()

  newBallot b = PaxosT $ liftF $ NewBallot b id

  quorumReached s = PaxosT $ liftF $ QuorumReached s id

  storeBallot b = PaxosT $ liftF $ StoreBallot b ()
  getBallot = PaxosT $ liftF $ GetBallot id
  storeProposal p = PaxosT $ liftF $ StoreProposal p ()
  getProposal = PaxosT $ liftF $ GetProposal id

deriving instance (Show ballot, Show value) => Show (Message (PaxosT ballot nodeId value m) t)

instance Arbitrary ballot => Arbitrary (Message (PaxosT ballot nodeId value m) P.Propose) where
  arbitrary = Propose <$> arbitrary
  shrink (Propose s) = [Propose s' | s' <- shrink s]

instance Monad m => MonadState (P.ProposerState (PaxosT ballot nodeId value m)) (PaxosT ballot nodeId value m t) where
  get = PaxosT $ liftF $ GetProposerState id
  put p = PaxosT $ liftF $ SetProposerState p ()

instance Monad m => MonadLogger (PaxosT ballot nodeId value m t) where
  monadLoggerLog loc src lvl msg = PaxosT $ liftF $ Log loc src lvl (toLogStr msg) ()

instance HasBallot (Message (PaxosT ballot nodeId value m) P.Propose) ballot where
  ballot = to $ \case
    Propose b -> b

instance HasBallot (Message (PaxosT ballot nodeId value m) P.Promise) ballot where
  ballot = to $ \case
    Promise b _ -> b

instance MayHaveProposal (Message (PaxosT ballot nodeId value m) P.Promise) ballot value where
  maybeProposal = to $ \case
    Promise _ v -> v

instance HasBallot (Message (PaxosT ballot nodeId value m) P.Accept) ballot where
  ballot = to $ \case
    Accept b _ -> b

instance HasValue (Message (PaxosT ballot nodeId value m) P.Accept) value where
  value = to $ \case
    Accept _ v -> v

instance HasProposal (Message (PaxosT ballot nodeId value m) P.Accepted) ballot value where
  proposal = to $ \case
    Accepted p -> p
