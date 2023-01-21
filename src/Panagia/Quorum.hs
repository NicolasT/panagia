-- |
-- Module: Panagia.Quorum
-- Description: Utilities to decide whether a quorum has been reached
-- Copyright: (c) Nicolas Trangez, 2023
-- License: Apache-2.0
-- Maintainer: ikke@nicolast.be
-- Stability: alpha
--
-- This module exposes functions that can be used as predicates to decide
-- whether a quorum has been reached in a cluster, and properties which any
-- quorum (or pair of quorums) must adhere to, as defined in the TLA+ model
-- of Single-Decree Paxos, see
-- https://raw.githubusercontent.com/tlaplus/tlapm/581ebd8afbe927b2b7d31c62ccec3a3409d9d8ce/examples/paxos/Paxos.tla.
module Panagia.Quorum
  ( -- * Quorum predicates

    -- | These functions can tell you whether (for some type of quorum
    -- function) a quorum has been reached. They're meant to be partially
    -- applied up to the quorum set, e.g.:
    --
    -- >>> let acceptors = ["a", "b", "c"]
    -- >>> let isQuorum = simpleMajority acceptors
    --
    -- Then use
    --
    -- >>> isQuorum ["a"]
    -- False
    -- >>> isQuorum ["a", "c"]
    -- True
    simpleMajority,
    simpleMajority',
    weighted,
    weighted',

    -- * Properties of valid quorums

    -- | These are exported so they can be reused in test-suites for other
    -- implementations of quorum predicates, e.g., by running various
    -- @QuickCheck@ checks using them.
    prop_quorumAssumption1,
    prop_quorumAssumption2,
    prop_quorumNonEmpty,
  )
where

import Data.Foldable (foldMap', foldl', toList)
import Data.List (intersect, isSubsequenceOf)
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set

-- $setup
-- >>> import Data.List ((\\))
-- >>> import Data.Maybe (fromJust)

-- | Quorum for clusters where nodes have some weight assigned.
--
-- === __Example__
-- A cluster of five nodes, three in one datacenter, two in another. We always
-- want (at least) one replica in each datacenter to reach consensus, and
-- tolerate the loss of up to two nodes (of course, not the two in the second
-- datacenter, since this would break the prerequisite).
--
-- >>> :{
-- let
--   nodeWeights = [
--       ("dc1a", 2), ("dc1b", 2), ("dc1c", 2),
--       ("dc2a", 3), ("dc2b", 3)
--       ]
--   nodes = map fst nodeWeights
--   nodeWeight = fromJust . flip lookup nodeWeights
-- :}
--
-- In a real application, the weight of a node is likely stored in the node
-- object, and hence retrieving its weight is a field lookup. This would make
-- this example quite verbose, hence the implementation above.
--
-- Now, some lookups:
--
-- - No replica in the second datacenter
--
-- >>> weighted nodeWeight nodes ["dc1a", "dc1b", "dc1c"]
-- False
--
-- - No replica in the first datacenter
--
-- >>> weighted nodeWeight nodes ["dc2a", "dc2b"]
-- False
--
-- - One replica in each datacenter, but other nodes could form a quorum
--
-- >>> weighted nodeWeight nodes ["dc1a", "dc2a"]
-- False
-- >>> weighted nodeWeight nodes (nodes \\ ["dc1a", "dc2a"])
-- True
--
-- - At least one replica in each datacenter, leftover nodes don't form a
--   quorum
--
-- >>> weighted nodeWeight nodes ["dc1a", "dc1b", "dc2a"]
-- True
-- >>> weighted nodeWeight nodes (nodes \\ ["dc1a", "dc1b", "dc2a"])
-- False
--
-- - At least one replica in each datacenter, leftover nodes don't form a
--   quorum
--
-- >>> weighted nodeWeight nodes ["dc1c", "dc2a", "dc2b"]
-- True
-- >>> weighted nodeWeight nodes (nodes \\ ["dc1c", "dc2a", "dc2b"])
-- False
weighted ::
  (Foldable f, Eq a, Integral w) =>
  -- | Function to lookup the weight of a given node
  (a -> w) ->
  -- | Collection of all nodes in the system. This should have set-like semantics.
  f a ->
  -- | Collection of nodes which may form a quorum. This should have set-like semantics.
  f a ->
  -- | Have we reached quorum?
  Bool
weighted getWeight acceptors maybeQuorum =
  maybeQuorum `isSubsetOf` acceptors -- to satisfy prop_quorumAssumption1
    && totalWeight maybeQuorum > totalWeight acceptors `div` 2
  where
    a `isSubsetOf` b = all (`elem` b) a
    totalWeight = getSum . foldMap' (Sum . getWeight)

-- | A version of 'weighted' specialized for 'Set's
weighted' :: (Ord a, Integral w) => (a -> w) -> Set a -> Set a -> Bool
weighted' getWeight acceptors maybeQuorum =
  maybeQuorum `Set.isSubsetOf` acceptors
    && totalWeight maybeQuorum > totalWeight acceptors `div` 2
  where
    totalWeight = foldl' (\acc a -> acc + getWeight a) 0

-- | Quorum by simple majority.
--
-- A quorum is reached when more than one half of the nodes in the system is
-- part of it: @length quorum >= length acceptors \`div\` 2 + 1@.
--
-- Internally, this is implemented using 'weighted' by assigning a constant
-- weight to every node.
--
-- === __Example__
-- Let's assume we have a cluster of three nodes:
--
-- >>> let nodes = ["a", "b", "c"]
--
-- Two nodes out of three form a quorum:
--
-- >>> simpleMajority nodes ["a", "b"]
-- True
--
-- One node doesn't:
--
-- >>> simpleMajority nodes ["c"]
-- False
--
-- Note, if a node is in the (potential) quorum, but not in the set of nodes,
-- a quorum isn't reached, even if without said node a quorum *would* be
-- reached:
--
-- >>> simpleMajority nodes ["c", "d"]
-- False
-- >>> simpleMajority nodes ["a", "b", "c", "d"]
-- False
simpleMajority ::
  (Foldable f, Eq a) =>
  -- | Collection of all nodes in the system. This should have set-like semantics.
  f a ->
  -- | Collection of nodes which may form a quorum. This should have set-like semantics.
  f a ->
  -- | Have we reached quorum?
  Bool
simpleMajority = weighted (const (1 :: Word))

-- | A version of 'simpleMajority' specialized for 'Set's
simpleMajority' :: Ord a => Set a -> Set a -> Bool
simpleMajority' acceptors maybeQuorum =
  maybeQuorum `Set.isSubsetOf` acceptors
    && Set.size maybeQuorum >= Set.size acceptors `div` 2 + 1

-- | @Quorums \\subseteq SUBSET Acceptors@
--
-- A quorum is a subset of all acceptors in the system.
prop_quorumAssumption1 ::
  (Foldable f, Eq a) =>
  -- | Collection of all nodes in the system (@Acceptors@). This should have set-like semantics.
  f a ->
  -- | Collection of nodes which form a quorum (a member of @Quorums@). This should have set-like semantics.
  f a ->
  -- | Does the property hold?
  Bool
prop_quorumAssumption1 acceptors quorum =
  toList quorum `isSubsequenceOf` toList acceptors

-- | @\\A Q1, Q2 \\in Quorums : Q1 \\cap Q2 # {}@
--
-- The intersection of two quorums can't be empty.
prop_quorumAssumption2 ::
  (Foldable f, Eq a) =>
  -- | A valid quorum in some universe (@Q1@). This should have set-like semantics.
  f a ->
  -- | A valid quorum in some universe (@Q2@). This should have set-like semantics.
  f a ->
  -- | Does the property hold?
  Bool
prop_quorumAssumption2 quorum1 quorum2 =
  not $ null $ toList quorum1 `intersect` toList quorum2

-- | @LEMMA QuorumNonEmpty == \\A Q \\in Quorums : Q # {} BY QuorumAssumption@
--
-- A quorum can't be empty. Indeed, this follows from 'prop_quorumAssumption2'.
prop_quorumNonEmpty ::
  Foldable f =>
  -- | A valid quorum in some universe (@Q@). This should have set-like semantics.
  f a ->
  -- | Does the property hold?
  Bool
prop_quorumNonEmpty = not . null
