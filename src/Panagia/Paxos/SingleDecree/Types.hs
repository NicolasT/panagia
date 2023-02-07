-- |
-- Module: Panagia.Paxos.SingleDecree.Types
-- Description: Type definitions for "Panagia.Paxos.SingleDecree"
-- Copyright: (c) Nicolas Trangez, 2023
-- License: GPL-3.0-only
-- Maintainer: ikke@nicolast.be
-- Stability: alpha
--
-- Type definitions (aliases) for structures used to implement Single-Decree
-- Paxos, and some utility types.
module Panagia.Paxos.SingleDecree.Types
  ( -- * Messages exchanged between nodes in the cluster

    -- ** From Proposers to Acceptors
    Prepare,
    Accept,

    -- ** From Acceptors to Proposers
    Promise,

    -- ** From Acceptors to Learners
    Accepted,

    -- * Miscellaneous
    Vote,
    InvariantViolation (..),
    invariantViolation,
  )
where

import Control.Exception.Base (Exception (..))
import Control.Monad.Catch (MonadThrow, throwM)

-- | Type of a vote.
type Vote ballot value = (ballot, value)

-- | Prepare message payload, for a given ballot type.
type Prepare ballot = ballot

-- | Promise message payload, for given ballot and value types.
type Promise ballot value = (ballot, Maybe (Vote ballot value))

-- | Accept message payload, for given ballot and value types.
type Accept ballot value = (ballot, value)

-- | Accepted message payload, for given ballot and value types.
type Accepted ballot value = (ballot, value)

-- | Exception thrown when some invariant is violated.
--
-- If this happens, there's likely an implementation bug somewhere, and
-- the process should likely abort execution.
newtype InvariantViolation = InvariantViolation String
  deriving (Show, Eq)

instance Exception InvariantViolation where
  displayException (InvariantViolation e) = "Invariant violation: " <> e

-- | 'throwM' an 'InvariantViolation'.
invariantViolation :: (MonadThrow m) => String -> m a
invariantViolation = throwM . InvariantViolation
