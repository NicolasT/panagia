{-# LANGUAGE LambdaCase #-}

module Panagia.Paxos.SingleDecree.Test.Utils
  ( eventually,
    receiveMin,
    receiveMax,
    receiveRandom,
    duplicating,
    dropping,
    annotating,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Hedgehog (MonadTest, PropertyT, annotateShow, forAll)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

eventually :: (Monad m) => m Bool -> m () -> m ()
eventually check act =
  check >>= \case
    True -> return ()
    False -> act >>= \() -> eventually check act

receiveMin :: (Ord a, Applicative m) => Set a -> m (Maybe a, Set a)
receiveMin s = case Set.minView s of
  Nothing -> pure (Nothing, mempty)
  Just (a, r) -> pure (Just a, r)

receiveMax :: (Ord a, Applicative m) => Set a -> m (Maybe a, Set a)
receiveMax s = case Set.maxView s of
  Nothing -> pure (Nothing, mempty)
  Just (a, r) -> pure (Just a, r)

receiveRandom :: (Monad m, Ord a, Show a) => Set a -> PropertyT m (Maybe a, Set a)
receiveRandom s
  | Set.null s = pure (Nothing, mempty)
  | otherwise = do
      idx <- forAll $ Gen.int (Range.constant 0 (Set.size s - 1))
      let e = Set.elemAt idx s
          rest = Set.deleteAt idx s
      annotateShow e
      pure (Just e, rest)

duplicating :: (Monad m, Show a, Ord a) => Double -> (Maybe a, Set a) -> PropertyT m (Maybe a, Set a)
duplicating freq = \case
  (Nothing, r) -> pure (Nothing, r)
  (Just msg, r) ->
    forAll $
      Gen.frequency
        [ (d, pure (Just msg, Set.insert msg r)),
          (100 - d, pure (Just msg, r))
        ]
  where
    d = floor (100 * freq)

dropping :: (Monad m, Show a) => Double -> (Maybe a, Set a) -> PropertyT m (Maybe a, Set a)
dropping freq = \case
  (Nothing, r) -> pure (Nothing, r)
  (Just msg, r) ->
    forAll $
      Gen.frequency
        [ (d, pure (Nothing, r)),
          (100 - d, pure (Just msg, r))
        ]
  where
    d = floor (100 * freq)

annotating :: (MonadTest m, Show a) => (Maybe a, Set a) -> m (Maybe a, Set a)
annotating (msg, s) = do
  annotateShow msg
  pure (msg, s)
