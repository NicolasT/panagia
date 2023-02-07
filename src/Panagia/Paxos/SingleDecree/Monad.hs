{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Panagia.Paxos.SingleDecree.Monad
-- Description: Monadic contexts in which "Panagia.Paxos.SingleDecree" actions run
-- Copyright: (c) Nicolas Trangez, 2023
-- License: GPL-3.0-only
-- Maintainer: ikke@nicolast.be
-- Stability: alpha
module Panagia.Paxos.SingleDecree.Monad
  ( MonadProposer (..),
    monadProposerLaws,
    MonadAcceptor (..),
    MonadAcceptorTransaction (..),
    monadAcceptorLaws,
  )
where

import Control.Monad.Catch.Pure (CatchT)
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.CPS as CPS (RWST)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Select (SelectT)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import qualified Control.Monad.Trans.Writer.CPS as CPS (WriterT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)
import Data.Kind (Type)
import Data.Maybe (isNothing)
import Data.Set (Set)
import Panagia.Paxos.SingleDecree.Types (Accept, Accepted, Prepare, Promise, Vote)
import Test.Lawful.Types (Laws, assert, discard)

-- MonadProposer {{{

-- | Monad providing the effects a Proposer needs to perform its duties.
class (Monad m) => MonadProposer m where
  -- | Type of an acceptor node in the cluster.
  type ProposerAcceptorNode m

  -- | Type of ballots used in the cluster.
  type ProposerBallot m

  -- | Type of the value on which to reach consensus.
  type ProposerValue m

  -- | Get a new ballot.
  --
  -- This ballot must be unique to this Proposer, must be strict-monotonic
  -- increasing, and should never be re-used. Hence, an implementation of
  -- 'newBallot' requires persistent state to stable storage.
  newBallot :: m (ProposerBallot m)
  default newBallot ::
    ( MonadProposer n,
      MonadTrans t,
      m ~ t n,
      ProposerBallot m ~ ProposerBallot n
    ) =>
    m (ProposerBallot m)
  newBallot = lift newBallot

  -- | Broadcast a 'Prepare' message to all Acceptors in the cluster.
  broadcastPrepare :: Prepare (ProposerBallot m) -> m ()
  default broadcastPrepare ::
    ( MonadProposer n,
      MonadTrans t,
      m ~ t n,
      ProposerBallot m ~ ProposerBallot n
    ) =>
    Prepare (ProposerBallot m) ->
    m ()
  broadcastPrepare = lift . broadcastPrepare

  -- | Broadcast an 'Accept' message to all Acceptors in the cluster.
  broadcastAccept :: Accept (ProposerBallot m) (ProposerValue m) -> m ()
  default broadcastAccept ::
    ( MonadProposer n,
      MonadTrans t,
      m ~ t n,
      ProposerBallot m ~ ProposerBallot n,
      ProposerValue m ~ ProposerValue n
    ) =>
    Accept (ProposerBallot m) (ProposerValue m) ->
    m ()
  broadcastAccept = lift . broadcastAccept

  -- | Check whether a given set of nodes forms a quorum in the cluster.
  isProposerQuorum :: Set (ProposerAcceptorNode m) -> m Bool
  default isProposerQuorum ::
    ( MonadProposer n,
      MonadTrans t,
      m ~ t n,
      ProposerAcceptorNode m ~ ProposerAcceptorNode n
    ) =>
    Set (ProposerAcceptorNode m) ->
    m Bool
  isProposerQuorum = lift . isProposerQuorum

instance (MonadProposer m, Monoid w) => MonadProposer (AccumT w m) where
  type ProposerAcceptorNode (AccumT w m) = ProposerAcceptorNode m
  type ProposerBallot (AccumT w m) = ProposerBallot m
  type ProposerValue (AccumT w m) = ProposerValue m

instance (MonadProposer m) => MonadProposer (ContT r m) where
  type ProposerAcceptorNode (ContT r m) = ProposerAcceptorNode m
  type ProposerBallot (ContT r m) = ProposerBallot m
  type ProposerValue (ContT r m) = ProposerValue m

instance (MonadProposer m) => MonadProposer (ExceptT e m) where
  type ProposerAcceptorNode (ExceptT e m) = ProposerAcceptorNode m
  type ProposerBallot (ExceptT e m) = ProposerBallot m
  type ProposerValue (ExceptT e m) = ProposerValue m

instance (MonadProposer m) => MonadProposer (IdentityT m) where
  type ProposerAcceptorNode (IdentityT m) = ProposerAcceptorNode m
  type ProposerBallot (IdentityT m) = ProposerBallot m
  type ProposerValue (IdentityT m) = ProposerValue m

instance (MonadProposer m) => MonadProposer (MaybeT m) where
  type ProposerAcceptorNode (MaybeT m) = ProposerAcceptorNode m
  type ProposerBallot (MaybeT m) = ProposerBallot m
  type ProposerValue (MaybeT m) = ProposerValue m

instance (MonadProposer m) => MonadProposer (CPS.RWST r w s m) where
  type ProposerAcceptorNode (CPS.RWST r w s m) = ProposerAcceptorNode m
  type ProposerBallot (CPS.RWST r w s m) = ProposerBallot m
  type ProposerValue (CPS.RWST r w s m) = ProposerValue m

instance (MonadProposer m, Monoid w) => MonadProposer (Lazy.RWST r w s m) where
  type ProposerAcceptorNode (Lazy.RWST r w s m) = ProposerAcceptorNode m
  type ProposerBallot (Lazy.RWST r w s m) = ProposerBallot m
  type ProposerValue (Lazy.RWST r w s m) = ProposerValue m

instance (MonadProposer m, Monoid w) => MonadProposer (Strict.RWST r w s m) where
  type ProposerAcceptorNode (Strict.RWST r w s m) = ProposerAcceptorNode m
  type ProposerBallot (Strict.RWST r w s m) = ProposerBallot m
  type ProposerValue (Strict.RWST r w s m) = ProposerValue m

instance (MonadProposer m) => MonadProposer (ReaderT r m) where
  type ProposerAcceptorNode (ReaderT r m) = ProposerAcceptorNode m
  type ProposerBallot (ReaderT r m) = ProposerBallot m
  type ProposerValue (ReaderT r m) = ProposerValue m

instance (MonadProposer m) => MonadProposer (SelectT r m) where
  type ProposerAcceptorNode (SelectT r m) = ProposerAcceptorNode m
  type ProposerBallot (SelectT r m) = ProposerBallot m
  type ProposerValue (SelectT r m) = ProposerValue m

instance (MonadProposer m) => MonadProposer (Lazy.StateT s m) where
  type ProposerAcceptorNode (Lazy.StateT s m) = ProposerAcceptorNode m
  type ProposerBallot (Lazy.StateT s m) = ProposerBallot m
  type ProposerValue (Lazy.StateT s m) = ProposerValue m

instance (MonadProposer m) => MonadProposer (Strict.StateT s m) where
  type ProposerAcceptorNode (Strict.StateT s m) = ProposerAcceptorNode m
  type ProposerBallot (Strict.StateT s m) = ProposerBallot m
  type ProposerValue (Strict.StateT s m) = ProposerValue m

instance (MonadProposer m) => MonadProposer (CPS.WriterT w m) where
  type ProposerAcceptorNode (CPS.WriterT w m) = ProposerAcceptorNode m
  type ProposerBallot (CPS.WriterT w m) = ProposerBallot m
  type ProposerValue (CPS.WriterT w m) = ProposerValue m

instance (MonadProposer m, Monoid w) => MonadProposer (Lazy.WriterT w m) where
  type ProposerAcceptorNode (Lazy.WriterT w m) = ProposerAcceptorNode m
  type ProposerBallot (Lazy.WriterT w m) = ProposerBallot m
  type ProposerValue (Lazy.WriterT w m) = ProposerValue m

instance (MonadProposer m, Monoid w) => MonadProposer (Strict.WriterT w m) where
  type ProposerAcceptorNode (Strict.WriterT w m) = ProposerAcceptorNode m
  type ProposerBallot (Strict.WriterT w m) = ProposerBallot m
  type ProposerValue (Strict.WriterT w m) = ProposerValue m

instance (MonadProposer m) => MonadProposer (CatchT m) where
  type ProposerAcceptorNode (CatchT m) = ProposerAcceptorNode m
  type ProposerBallot (CatchT m) = ProposerBallot m
  type ProposerValue (CatchT m) = ProposerValue m

-- | Laws for 'MonadProposer' instances.
monadProposerLaws :: (MonadProposer m, Ord (ProposerBallot m)) => Laws m
monadProposerLaws =
  [ ( "newBallot generates monotonic increasing values",
      Just <$> do
        b1 <- newBallot
        b2 <- newBallot
        b3 <- newBallot
        return $ b2 > b1 && b3 > b2
    )
  ]

-- }}}

-- MonadAcceptorTransaction {{{

-- | Monad providing the effects an Accepter needs towards its persistent
-- state to perform its duties.
class (Monad m) => MonadAcceptorTransaction ballot value m | m -> ballot, m -> value where
  -- | Get the ballot previously stored using 'setMaxBallot', if one was stored
  -- before.
  getMaxBallot :: m (Maybe ballot)
  default getMaxBallot ::
    ( MonadAcceptorTransaction ballot value n,
      MonadTrans t,
      m ~ t n
    ) =>
    m (Maybe ballot)
  getMaxBallot = lift getMaxBallot

  -- | Store the "max ballot". This must be persisted to stable storage.
  setMaxBallot :: ballot -> m ()
  default setMaxBallot ::
    ( MonadAcceptorTransaction ballot value n,
      MonadTrans t,
      m ~ t n
    ) =>
    ballot ->
    m ()
  setMaxBallot = lift . setMaxBallot

  -- | Get the "vote" previously stored using 'setVote', if one was stored before.
  getVote :: m (Maybe (Vote ballot value))
  default getVote ::
    ( MonadAcceptorTransaction ballot value n,
      MonadTrans t,
      m ~ t n
    ) =>
    m (Maybe (Vote ballot value))
  getVote = lift getVote

  -- | Store the "vote". This must be persisted to stable storage.
  setVote :: Vote ballot value -> m ()
  default setVote ::
    ( MonadAcceptorTransaction ballot value n,
      MonadTrans t,
      m ~ t n
    ) =>
    Vote ballot value ->
    m ()
  setVote = lift . setVote

instance (MonadAcceptorTransaction ballot value m, Monoid w) => MonadAcceptorTransaction ballot value (AccumT w m)

instance (MonadAcceptorTransaction ballot value m) => MonadAcceptorTransaction ballot value (ContT r m)

instance (MonadAcceptorTransaction ballot value m) => MonadAcceptorTransaction ballot value (ExceptT e m)

instance (MonadAcceptorTransaction ballot value m) => MonadAcceptorTransaction ballot value (IdentityT m)

instance (MonadAcceptorTransaction ballot value m) => MonadAcceptorTransaction ballot value (MaybeT m)

instance (MonadAcceptorTransaction ballot value m) => MonadAcceptorTransaction ballot value (CPS.RWST r w s m)

instance (MonadAcceptorTransaction ballot value m, Monoid w) => MonadAcceptorTransaction ballot value (Lazy.RWST r w s m)

instance (MonadAcceptorTransaction ballot value m, Monoid w) => MonadAcceptorTransaction ballot value (Strict.RWST r w s m)

instance (MonadAcceptorTransaction ballot value m) => MonadAcceptorTransaction ballot value (ReaderT r m)

instance (MonadAcceptorTransaction ballot value m) => MonadAcceptorTransaction ballot value (SelectT r m)

instance (MonadAcceptorTransaction ballot value m) => MonadAcceptorTransaction ballot value (Lazy.StateT s m)

instance (MonadAcceptorTransaction ballot value m) => MonadAcceptorTransaction ballot value (Strict.StateT s m)

instance (MonadAcceptorTransaction ballot value m) => MonadAcceptorTransaction ballot value (CPS.WriterT w m)

instance (MonadAcceptorTransaction ballot value m, Monoid w) => MonadAcceptorTransaction ballot value (Lazy.WriterT w m)

instance (MonadAcceptorTransaction ballot value m, Monoid w) => MonadAcceptorTransaction ballot value (Strict.WriterT w m)

instance (MonadAcceptorTransaction ballot value m) => MonadAcceptorTransaction ballot value (CatchT m)

-- }}}

-- MonadAcceptor {{{

-- | Monad providing the effects an Acceptor needs to perform its duties.
class (Monad m, MonadAcceptorTransaction (AcceptorBallot m) (AcceptorValue m) (AcceptorTransaction m)) => MonadAcceptor m where
  -- | Type of a proposer node in the cluster.
  type AcceptorProposerNode m

  -- | Type of ballots used in the cluster.
  type AcceptorBallot m

  -- | Type of the value on which to reach consensus.
  type AcceptorValue m

  -- | Type of a 'MonadAcceptorTransaction' in which a transaction against the
  -- persistent state can be performed.
  type AcceptorTransaction m :: Type -> Type

  -- | Run a transaction against the persistent state.
  --
  -- If an error occurs within the given 'AcceptorTransaction', the transaction
  -- must be rolled back.
  transactionally :: AcceptorTransaction m a -> m a
  default transactionally ::
    ( MonadAcceptor n,
      MonadTrans t,
      m ~ t n,
      AcceptorTransaction m ~ AcceptorTransaction n
    ) =>
    AcceptorTransaction m a ->
    m a
  transactionally = lift . transactionally

  -- | Send a 'Promise' message to the given node.
  sendPromise :: AcceptorProposerNode m -> Promise (AcceptorBallot m) (AcceptorValue m) -> m ()
  default sendPromise ::
    ( MonadAcceptor n,
      MonadTrans t,
      m ~ t n,
      AcceptorProposerNode m ~ AcceptorProposerNode n,
      AcceptorBallot m ~ AcceptorBallot n,
      AcceptorValue m ~ AcceptorValue n
    ) =>
    AcceptorProposerNode m ->
    Promise (AcceptorBallot m) (AcceptorValue m) ->
    m ()
  sendPromise n p = lift $ sendPromise n p

  -- | Broadcast an 'Accepted' message to all Learners in the cluster.
  broadcastAccepted :: Accepted (AcceptorBallot m) (AcceptorValue m) -> m ()
  default broadcastAccepted ::
    ( MonadAcceptor n,
      MonadTrans t,
      m ~ t n,
      AcceptorBallot m ~ AcceptorBallot n,
      AcceptorValue m ~ AcceptorValue n
    ) =>
    Accepted (AcceptorBallot m) (AcceptorValue m) ->
    m ()
  broadcastAccepted = lift . broadcastAccepted

instance (MonadAcceptor m, Monoid w) => MonadAcceptor (AccumT w m) where
  type AcceptorProposerNode (AccumT w m) = AcceptorProposerNode m
  type AcceptorBallot (AccumT w m) = AcceptorBallot m
  type AcceptorValue (AccumT w m) = AcceptorValue m
  type AcceptorTransaction (AccumT w m) = AcceptorTransaction m

instance (MonadAcceptor m) => MonadAcceptor (ContT r m) where
  type AcceptorProposerNode (ContT r m) = AcceptorProposerNode m
  type AcceptorBallot (ContT r m) = AcceptorBallot m
  type AcceptorValue (ContT r m) = AcceptorValue m
  type AcceptorTransaction (ContT r m) = AcceptorTransaction m

instance (MonadAcceptor m) => MonadAcceptor (ExceptT e m) where
  type AcceptorProposerNode (ExceptT e m) = AcceptorProposerNode m
  type AcceptorBallot (ExceptT e m) = AcceptorBallot m
  type AcceptorValue (ExceptT e m) = AcceptorValue m
  type AcceptorTransaction (ExceptT e m) = AcceptorTransaction m

instance (MonadAcceptor m) => MonadAcceptor (IdentityT m) where
  type AcceptorProposerNode (IdentityT m) = AcceptorProposerNode m
  type AcceptorBallot (IdentityT m) = AcceptorBallot m
  type AcceptorValue (IdentityT m) = AcceptorValue m
  type AcceptorTransaction (IdentityT m) = AcceptorTransaction m

instance (MonadAcceptor m) => MonadAcceptor (MaybeT m) where
  type AcceptorProposerNode (MaybeT m) = AcceptorProposerNode m
  type AcceptorBallot (MaybeT m) = AcceptorBallot m
  type AcceptorValue (MaybeT m) = AcceptorValue m
  type AcceptorTransaction (MaybeT m) = AcceptorTransaction m

instance (MonadAcceptor m) => MonadAcceptor (CPS.RWST r w s m) where
  type AcceptorProposerNode (CPS.RWST r w s m) = AcceptorProposerNode m
  type AcceptorBallot (CPS.RWST r w s m) = AcceptorBallot m
  type AcceptorValue (CPS.RWST r w s m) = AcceptorValue m
  type AcceptorTransaction (CPS.RWST r w s m) = AcceptorTransaction m

instance (MonadAcceptor m, Monoid w) => MonadAcceptor (Lazy.RWST r w s m) where
  type AcceptorProposerNode (Lazy.RWST r w s m) = AcceptorProposerNode m
  type AcceptorBallot (Lazy.RWST r w s m) = AcceptorBallot m
  type AcceptorValue (Lazy.RWST r w s m) = AcceptorValue m
  type AcceptorTransaction (Lazy.RWST r w s m) = AcceptorTransaction m

instance (MonadAcceptor m, Monoid w) => MonadAcceptor (Strict.RWST r w s m) where
  type AcceptorProposerNode (Strict.RWST r w s m) = AcceptorProposerNode m
  type AcceptorBallot (Strict.RWST r w s m) = AcceptorBallot m
  type AcceptorValue (Strict.RWST r w s m) = AcceptorValue m
  type AcceptorTransaction (Strict.RWST r w s m) = AcceptorTransaction m

instance (MonadAcceptor m) => MonadAcceptor (ReaderT r m) where
  type AcceptorProposerNode (ReaderT r m) = AcceptorProposerNode m
  type AcceptorBallot (ReaderT r m) = AcceptorBallot m
  type AcceptorValue (ReaderT r m) = AcceptorValue m
  type AcceptorTransaction (ReaderT r m) = AcceptorTransaction m

instance (MonadAcceptor m) => MonadAcceptor (SelectT r m) where
  type AcceptorProposerNode (SelectT r m) = AcceptorProposerNode m
  type AcceptorBallot (SelectT r m) = AcceptorBallot m
  type AcceptorValue (SelectT r m) = AcceptorValue m
  type AcceptorTransaction (SelectT r m) = AcceptorTransaction m

instance (MonadAcceptor m) => MonadAcceptor (Lazy.StateT s m) where
  type AcceptorProposerNode (Lazy.StateT s m) = AcceptorProposerNode m
  type AcceptorBallot (Lazy.StateT s m) = AcceptorBallot m
  type AcceptorValue (Lazy.StateT s m) = AcceptorValue m
  type AcceptorTransaction (Lazy.StateT s m) = AcceptorTransaction m

instance (MonadAcceptor m) => MonadAcceptor (Strict.StateT s m) where
  type AcceptorProposerNode (Strict.StateT s m) = AcceptorProposerNode m
  type AcceptorBallot (Strict.StateT s m) = AcceptorBallot m
  type AcceptorValue (Strict.StateT s m) = AcceptorValue m
  type AcceptorTransaction (Strict.StateT s m) = AcceptorTransaction m

instance (MonadAcceptor m) => MonadAcceptor (CPS.WriterT w m) where
  type AcceptorProposerNode (CPS.WriterT w m) = AcceptorProposerNode m
  type AcceptorBallot (CPS.WriterT w m) = AcceptorBallot m
  type AcceptorValue (CPS.WriterT w m) = AcceptorValue m
  type AcceptorTransaction (CPS.WriterT w m) = AcceptorTransaction m

instance (MonadAcceptor m, Monoid w) => MonadAcceptor (Lazy.WriterT w m) where
  type AcceptorProposerNode (Lazy.WriterT w m) = AcceptorProposerNode m
  type AcceptorBallot (Lazy.WriterT w m) = AcceptorBallot m
  type AcceptorValue (Lazy.WriterT w m) = AcceptorValue m
  type AcceptorTransaction (Lazy.WriterT w m) = AcceptorTransaction m

instance (MonadAcceptor m, Monoid w) => MonadAcceptor (Strict.WriterT w m) where
  type AcceptorProposerNode (Strict.WriterT w m) = AcceptorProposerNode m
  type AcceptorBallot (Strict.WriterT w m) = AcceptorBallot m
  type AcceptorValue (Strict.WriterT w m) = AcceptorValue m
  type AcceptorTransaction (Strict.WriterT w m) = AcceptorTransaction m

instance (MonadAcceptor m) => MonadAcceptor (CatchT m) where
  type AcceptorProposerNode (CatchT m) = AcceptorProposerNode m
  type AcceptorBallot (CatchT m) = AcceptorBallot m
  type AcceptorValue (CatchT m) = AcceptorValue m
  type AcceptorTransaction (CatchT m) = AcceptorTransaction m

-- | Laws for 'MonadAcceptor' instances.
monadAcceptorLaws ::
  ( MonadAcceptor m,
    Eq (AcceptorBallot m),
    Eq (AcceptorValue m)
  ) =>
  m (AcceptorBallot m) ->
  m (AcceptorValue m) ->
  Laws m
monadAcceptorLaws genBallot genValue =
  [ ( "getMaxBallot returns Nothing until something is set",
      do
        b <- transactionally getMaxBallot
        assert $ isNothing b
    ),
    ( "getMaxBallot yields what was set using setMaxBallot",
      do
        b0 <- transactionally getMaxBallot
        b <- genBallot

        if Just b == b0
          then discard
          else transactionally $ do
            setMaxBallot b
            b' <- getMaxBallot
            assert $ b' == Just b
    ),
    ( "getVote returns Nothing until something is set",
      do
        v <- transactionally getVote
        assert $ isNothing v
    ),
    ( "getVote returns what was set using setVote",
      do
        v0 <- transactionally getVote
        b <- genBallot
        v <- genValue

        if Just (b, v) == v0
          then discard
          else transactionally $ do
            setVote (b, v)
            v' <- getVote
            assert $ v' == Just (b, v)
    )
  ]

-- }}}
