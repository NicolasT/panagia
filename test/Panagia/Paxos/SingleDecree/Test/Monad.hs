{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Panagia.Paxos.SingleDecree.Test.Monad
  ( evalTestT,
    initProposer,
    resetLearner,
    handleMessages,
    handleMessage,
    consensusReached,
    tests,
  )
where

import Control.Lens
  ( assign,
    at,
    ix,
    preuse,
    use,
    uses,
    view,
    zoom,
    (&),
    (.=),
    (.~),
    (<>=),
    (<~),
    (?=),
    (??),
    _Just,
    _Left,
  )
import Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import Control.Monad.Catch.Pure (runCatch)
import Control.Monad.Free (iterM)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free (FreeT, iterT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT, runStateT)
import Control.Monad.Trans.Writer (WriterT, runWriterT)
import Control.Monad.Writer.Class (MonadWriter, tell)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Some (Some, mkSome, withSomeM)
import Hedgehog (PropertyT, forAll)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Panagia.Paxos.SingleDecree as S
import qualified Panagia.Paxos.SingleDecree.Free as F
import Panagia.Paxos.SingleDecree.Test.Types
  ( AcceptorPersistentState (..),
    Ballot (..),
    Config (..),
    Message (..),
    Node (..),
    NodeState (..),
    ProposerPersistentState (..),
    Role (..),
    State (..),
    acceptorMaxBallot,
    acceptorVote,
    acceptors,
    ballot0,
    configAcceptors,
    configLearners,
    isQuorum,
    learners,
    messages,
    persistentState,
    proposerBallot,
    proposers,
    succBallot,
    volatileState,
  )
import Test.Lawful.Hedgehog (testLaws)
import Test.Tasty (TestTree, testGroup)

type TestT value m = StateT (State value) (ReaderT Config m)

initProposer :: (MonadState (State value) m, MonadReader Config m, Ord value) => Node Proposer -> value -> m ()
initProposer p v = do
  st <- use (proposers . at p)
  let ps = maybe (ProposerPersistentState Nothing) (view persistentState) st
  ((s, st'), msgs) <- runWriterT $ flip runStateT ps $ runProposer p (S.propose v)
  proposers . at p ?= ProposerState st' s
  messages <>= msgs

resetLearner :: (MonadState (State value) m) => Node Learner -> m ()
resetLearner node = learners . at node . _Just . volatileState .= Right (S.initLearnerState 2)

handleMessage ::
  ( MonadCatch m,
    Ord value
  ) =>
  Message value to ->
  TestT value m ()
handleMessage msg = do
  ((), msgs) <- runWriterT $ case msg of
    Prepare f t m -> zoom (acceptors . ix t . persistentState) $ runAcceptor t (S.handlePrepare f m)
    Promise f t m -> zoom (proposers . ix t) $ do
      st <- use volatileState
      volatileState <~ zoom persistentState (runProposer t (S.handlePromise st f m))
    Accept _ t m -> zoom (acceptors . ix t . persistentState) $ runAcceptor t (S.handleAccept m)
    Accepted f t m -> zoom (learners . ix t) $ do
      st <- use volatileState
      case st of
        Left _ -> return () -- Reached consensus before, discard the message
        Right st' -> do
          volatileState <~ runLearner (S.handleAccepted st' f m)
  messages <>= msgs

runProposer ::
  ( MonadState ProposerPersistentState m,
    MonadReader Config m,
    MonadWriter (Set (Some (Message value))) m,
    Ord value
  ) =>
  Node Proposer ->
  FreeT (F.ProposerF (Node Acceptor) Ballot value) m a ->
  m a
runProposer thisNode = iterT $ \case
  F.NewBallot next -> do
    b <- uses proposerBallot (maybe (ballot0 thisNode) succBallot)
    proposerBallot ?= b
    next b
  F.BroadcastPrepare m next -> do
    acceptors' <- view configAcceptors
    tell $ Set.map (\t -> mkSome $ Prepare thisNode t m) acceptors'
    next
  F.BroadcastAccept m next -> do
    acceptors' <- view configAcceptors
    tell $ Set.map (\t -> mkSome $ Accept thisNode t m) acceptors'
    next
  F.IsProposerQuorum s next ->
    next . ($ s) =<< view isQuorum

runAcceptor ::
  ( MonadState (AcceptorPersistentState value) m,
    MonadReader Config m,
    MonadWriter (Set (Some (Message value))) m,
    MonadThrow m,
    Ord value
  ) =>
  Node Acceptor ->
  FreeT (F.AcceptorF (Node Proposer) Ballot value) m a ->
  m a
runAcceptor thisNode = iterT $ \case
  F.SendPromise t p next -> do
    tell $ Set.singleton $ mkSome $ Promise thisNode t p
    next
  F.BroadcastAccepted m next -> do
    learners' <- view configLearners
    tell $ Set.map (\t -> mkSome $ Accepted thisNode t m) learners'
    next
  F.Transactionally t next -> do
    st <- get
    let runT = flip iterM t $ \case
          F.GetMaxBallot next' -> use acceptorMaxBallot >>= next'
          F.SetMaxBallot b next' -> assign acceptorMaxBallot (Just b) >> next'
          F.GetVote next' -> use acceptorVote >>= next'
          F.SetVote v next' -> assign acceptorVote (Just v) >> next'

    case runCatch (runStateT runT st) of
      Left err -> throwM err
      Right (a, st') -> put st' >> next a

runLearner ::
  ( MonadReader Config m
  ) =>
  FreeT (F.LearnerF (Node Acceptor)) m a ->
  m a
runLearner = iterT $ \case
  F.IsLearnerQuorum s next ->
    next . ($ s) =<< view isQuorum

handleMessages ::
  (MonadCatch m, Ord value) =>
  (Set (Some (Message value)) -> m (Maybe (Some (Message value)), Set (Some (Message value)))) ->
  TestT value m ()
handleMessages pick = loop
  where
    loop = do
      msgs <- use messages
      if Set.null msgs
        then return ()
        else do
          (msg, msgs') <- lift $ lift $ pick msgs
          messages .= msgs'
          case msg of
            Nothing -> loop
            Just msg' -> do
              withSomeM (pure msg') handleMessage
              loop

evalTestT ::
  (Monad m, Ord value) =>
  (Set (Node Acceptor) -> Bool) ->
  Set (Node Acceptor) ->
  Set (Node Learner) ->
  TestT value m a ->
  m a
evalTestT q a l act = runReaderT (evalStateT act state0) config
  where
    config = Config a l q
    state0 =
      mempty
        & acceptors .~ Map.fromSet (const (AcceptorState $ AcceptorPersistentState Nothing Nothing)) a
        & learners .~ Map.fromSet (const (LearnerState $ Right $ S.initLearnerState 2)) l

consensusReached :: (MonadState (State value) m) => Node Learner -> m (Maybe value)
consensusReached node = preuse (learners . at node . _Just . volatileState . _Left)

tests :: TestTree
tests =
  testGroup
    "TestT"
    [ testLaws "monadProposerLaws" runProposer' S.monadProposerLaws,
      testLaws "monadAcceptorLaws" runAcceptor' (S.monadAcceptorLaws (forAll' genBallot) (forAll' genValue))
    ]
  where
    genBallot =
      Ballot
        <$> Gen.word64 Range.linearBounded
        <*> (ProposerNode <$> Gen.string (Range.constant 1 10) Gen.alphaNum)
    genValue = Gen.int Range.linearBounded
    forAll' = lift . lift . lift . lift . forAll
    runProposer' ::
      FreeT
        (F.ProposerF (Node 'Acceptor) Ballot Int)
        ( StateT
            ProposerPersistentState
            ( WriterT
                (Set (Some (Message Int)))
                (ReaderT Config Identity)
            )
        )
        a ->
      PropertyT IO a
    runProposer' act =
      pure
        $ fst . fst
        $ runReader
          ?? Config mempty mempty (const False)
        $ runWriterT
        $ runStateT
          ?? ProposerPersistentState Nothing
        $ runProposer "proposer" act
    runAcceptor' act = do
      ((a, _), _) <-
        runReaderT
          ?? Config mempty mempty (const False)
          $ runWriterT
          $ runStateT
            ?? AcceptorPersistentState Nothing Nothing
          $ runAcceptor "acceptor" act
      pure a
