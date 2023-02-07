{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Panagia.Paxos.SingleDecree.Free
-- Description: Functors for @free@ implementations of "Panagia.Paxos.SingleDecree" monads
-- Copyright: (c) Nicolas Trangez, 2023
-- License: GPL-3.0-only
-- Maintainer: ikke@nicolast.be
-- Stability: alpha
--
-- This module exports functors, 'ProposerF', 'AcceptorF' and 'TransactionF',
-- when used to construct a @free@ monad, can be used to implement 'MonadProposer',
-- 'MonadAcceptor' and 'MonadAcceptorTransaction'.
--
-- Hence, instances of said classes are provided for 'Free' and 'FreeT' applied
-- to the respective functors.
module Panagia.Paxos.SingleDecree.Free
  ( ProposerF (..),
    AcceptorF (..),
    TransactionF (..),
  )
where

import Control.Monad.Free (Free)
import Control.Monad.Free.Class (liftF)
import Control.Monad.Trans.Free (FreeT)
import Data.Set (Set)
import Panagia.Paxos.SingleDecree
  ( Accept,
    Accepted,
    MonadAcceptor (..),
    MonadAcceptorTransaction (..),
    MonadProposer (..),
    Prepare,
    Promise,
    Vote,
  )

-- | A functor representing 'MonadProposer' actions.
--
-- This can be used to construct (and evaluate) a @free@ monad, hence there
-- are 'MonadProposer' instances for, e.g., 'FreeT' 'ProposerF'.
data ProposerF node ballot value a
  = -- | 'newBallot'
    NewBallot (ballot -> a)
  | -- | 'broadcastPrepare'
    BroadcastPrepare (Prepare ballot) a
  | -- | 'broadcastAccept'
    BroadcastAccept (Accept ballot value) a
  | -- | 'isProposerQuorum'
    IsProposerQuorum (Set node) (Bool -> a)
  deriving (Functor)

instance MonadProposer (Free (ProposerF node ballot value)) where
  type ProposerAcceptorNode (Free (ProposerF node ballot value)) = node
  type ProposerBallot (Free (ProposerF node ballot value)) = ballot
  type ProposerValue (Free (ProposerF node ballot value)) = value

  newBallot = liftF $ NewBallot id
  broadcastPrepare b = liftF $ BroadcastPrepare b ()
  broadcastAccept msg = liftF $ BroadcastAccept msg ()
  isProposerQuorum s = liftF $ IsProposerQuorum s id

instance (Monad m) => MonadProposer (FreeT (ProposerF node ballot value) m) where
  type ProposerAcceptorNode (FreeT (ProposerF node ballot value) m) = node
  type ProposerBallot (FreeT (ProposerF node ballot value) m) = ballot
  type ProposerValue (FreeT (ProposerF node ballot value) m) = value

  newBallot = liftF $ NewBallot id
  broadcastPrepare b = liftF $ BroadcastPrepare b ()
  broadcastAccept msg = liftF $ BroadcastAccept msg ()
  isProposerQuorum s = liftF $ IsProposerQuorum s id

-- | A functor representing 'MonadAcceptorTransaction' actions.
--
-- This can be used to construct (and evaluate) a @free@ monad, hence there
-- are 'MonadAcceptorTransaction' instances for, e.g., 'Free' 'TransactionF'.
data TransactionF ballot value a
  = -- | 'getMaxBallot'
    GetMaxBallot (Maybe ballot -> a)
  | -- | 'setMaxBallot'
    SetMaxBallot ballot a
  | -- | 'getVote'
    GetVote (Maybe (Vote ballot value) -> a)
  | -- | 'setVote'
    SetVote (Vote ballot value) a
  deriving (Functor)

instance MonadAcceptorTransaction ballot value (Free (TransactionF ballot value)) where
  getMaxBallot = liftF $ GetMaxBallot id
  setMaxBallot b = liftF $ SetMaxBallot b ()
  getVote = liftF $ GetVote id
  setVote v = liftF $ SetVote v ()

instance (Monad m) => MonadAcceptorTransaction ballot value (FreeT (TransactionF ballot value) m) where
  getMaxBallot = liftF $ GetMaxBallot id
  setMaxBallot b = liftF $ SetMaxBallot b ()
  getVote = liftF $ GetVote id
  setVote v = liftF $ SetVote v ()

-- | A functor representing 'MonadAcceptor' actions.
--
-- This can be used to construct (and evaluate) a @free@ monad, hence there
-- are 'MonadAcceptor' instances for, e.g., 'FreeT' 'AcceptorF'.
--
-- The associated 'MonadAcceptorTransaction' is 'Free' 'TransactionF'.
data AcceptorF node ballot value a
  = -- | 'transactionally'
    forall b. Transactionally (Free (TransactionF ballot value) b) (b -> a)
  | -- | 'sendPromise'
    SendPromise node (Promise ballot value) a
  | -- | 'broadcastAccepted'
    BroadcastAccepted (Accepted ballot value) a

instance Functor (AcceptorF node ballot value) where
  fmap f = \case
    SendPromise n m a -> SendPromise n m (f a)
    BroadcastAccepted m a -> BroadcastAccepted m (f a)
    Transactionally t a -> Transactionally t (f . a)

instance MonadAcceptor (Free (AcceptorF node ballot value)) where
  type AcceptorProposerNode (Free (AcceptorF node ballot value)) = node
  type AcceptorBallot (Free (AcceptorF node ballot value)) = ballot
  type AcceptorValue (Free (AcceptorF node ballot value)) = value
  type AcceptorTransaction (Free (AcceptorF node ballot value)) = Free (TransactionF ballot value)

  transactionally t = liftF $ Transactionally t id
  sendPromise n msg = liftF $ SendPromise n msg ()
  broadcastAccepted msg = liftF $ BroadcastAccepted msg ()

instance (Monad m) => MonadAcceptor (FreeT (AcceptorF node ballot value) m) where
  type AcceptorProposerNode (FreeT (AcceptorF node ballot value) m) = node
  type AcceptorBallot (FreeT (AcceptorF node ballot value) m) = ballot
  type AcceptorValue (FreeT (AcceptorF node ballot value) m) = value
  type AcceptorTransaction (FreeT (AcceptorF node ballot value) m) = Free (TransactionF ballot value)

  transactionally t = liftF $ Transactionally t id
  sendPromise n msg = liftF $ SendPromise n msg ()
  broadcastAccepted msg = liftF $ BroadcastAccepted msg ()
