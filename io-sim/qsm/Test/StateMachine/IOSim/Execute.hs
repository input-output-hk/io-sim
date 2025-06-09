{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

module Test.StateMachine.IOSim.Execute
  ( -- * Executing
    runSequential
  , runSequentialException
  , runParallel
  , runParallelException
  ) where

import Control.Concurrent.Class.MonadSTM
import Control.Monad (when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTest
import Control.Monad.Class.MonadThrow
import Control.Monad.Error.Class
import Control.Monad.IOSim
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.CPS (runWriterT)
import Control.Monad.Writer.CPS
import Data.Bifunctor
import Data.Dynamic (Dynamic, toDyn)
import Data.Either
import Data.Either (fromLeft)
import Data.Foldable (foldl', for_, toList)
import Data.Function ((&))
import Data.Maybe
import Data.Sequence (Seq)
import Data.Set qualified as Set
import Test.QuickCheck
import Test.StateMachine.IOSim.Types
import Test.StateMachine.Logic
import Test.StateMachine.Types hiding (StateMachine (..))
import Test.StateMachine.Types.Rank2 qualified as Rank2
import Text.Show.Pretty (ppShow)

{-------------------------------------------------------------------------------
  Runner helpers
-------------------------------------------------------------------------------}

-- | Run an IOSim action
runIOSim ::
    (Show (resp Concrete)) =>
    (forall s. IOSim s (resp Concrete)) ->
    ( SimTrace (resp Concrete)
    , -- \^ Trace for printing
      resp Concrete
    , -- \^ Response
      Property
      -- \^ False if there was an exception thrown
    )
runIOSim act =
    let trace = runSimTrace act
        e msg err =
            ( trace
            , undefined
            , counterexample (ppTrace trace) $
                counterexample (msg <> show err) $
                    property False
            )
     in case traceResult False trace of
            Left (FailureException err) -> e "Exception thrown: " err
            Left err                    -> e "Internal IOSim error: " err
            Right v                     -> (trace, v, property True)

-- | Run an IOSimPOR action and check the property for each possible interleaving
runIOSimPOR ::
    (Show a) =>
    -- | Test to run
    (forall s. IOSim s a) ->
    ( SimTrace a ->
      -- \^ Trace
      Bool ->
      -- \^ Was there a previous successful trace?
      a ->
      -- \^ traceResult
      Property
    ) ->
    Property
runIOSimPOR act prop =
    exploreSimTrace id (exploreRaces >> act) $ \(isJust -> pt) trace ->
        let e msg err =
                counterexample (ppTrace trace) $
                    counterexample (msg <> show err) $
                        property False
         in case traceResult False trace of
                Left (FailureException err) ->
                    e
                        ( ( if pt
                                then ("Race condition found! " <>)
                                else id
                          )
                            "Exception thrown: "
                        )
                        err
                Left (FailureDeadlock err) ->
                    e
                        ( ( if pt
                                then ("Race condition found! " <>)
                                else id
                          )
                            "Deadlock found: "
                        )
                        err
                Left err ->
                    e
                        ( ( if pt
                                then ("Race condition found! " <>)
                                else id
                          )
                            "Internal IOSim error: "
                        )
                        err
                Right v -> prop trace pt v

-- | Run an IOSimPOR action and check the property for each possible interleaving
runIOSimPORAcceptExceptions ::
    (Show a) =>
    -- | Test to run
    (forall s. IOSim s a) ->
    ( SimTrace a ->
      -- \^ Trace
      Bool ->
      -- \^ Was there a previous successful trace?
      Either SomeException a ->
      -- \^ traceResult
      Property
    ) ->
    Property
runIOSimPORAcceptExceptions act prop =
    exploreSimTrace id (exploreRaces >> act) $ \(isJust -> pt) trace ->
        let e msg err =
                counterexample (ppTrace trace) $
                    counterexample (msg <> show err) $
                        property False
         in case traceResult False trace of
                Left (FailureDeadlock err) ->
                    e
                        ( ( if pt
                                then ("Race condition found! " <>)
                                else id
                          )
                            "Deadlock found: "
                        )
                        err
                Right v -> prop trace pt (Right v)
                Left (FailureException err) -> prop trace pt (Left err)
                Left err ->
                    e
                        ( ( if pt
                                then ("Race condition found! " <>)
                                else id
                          )
                            "Internal IOSim error: "
                        )
                        err

{-------------------------------------------------------------------------------
  Running functions
-------------------------------------------------------------------------------}

runCommandsSequentially ::
    forall model cmd m resp.
    ( Monad m
    , Show (cmd Concrete)
    , Show (resp Concrete)
    , Rank2.Traversable cmd
    , Rank2.Foldable resp
    ) =>
    -- | How to run the 'semantics'.
    (m (resp Concrete) -> QsmM model cmd resp m (resp Concrete)) ->
    StateMachine model cmd m resp ->
    Commands cmd resp ->
    QsmM model cmd resp m ()
runCommandsSequentially runSemantics sm (Commands scmds) =
    for_ scmds $ \(Command scmd _ vars) -> do
      QsmState{env, smodel, counter, cmodel} <- get
      checkLogic
        (logic (precondition sm smodel scmd))
        (PreconditionFailed . show)
      let ccmd = fromRight (error "impossible") (reify env scmd)
      logEvent $ Invocation ccmd (Set.fromList vars)
      cresp <- runSemantics $ semantics sm ccmd
      logEvent $ Response cresp
      let cvars = getUsedConcrete cresp
      when (length vars /= length cvars) $ do
        let err = mockSemanticsMismatchError
                    (ppShow ccmd)
                    (ppShow vars)
                    (ppShow cresp)
                    (ppShow cvars)
        finishWithReason $ MockSemanticsMismatch err
      checkLogic
        (logic (postcondition sm cmodel ccmd cresp))
        (PostconditionFailed . show)
      checkLogic
        (logic (fromMaybe (const Top) (invariant sm) cmodel))
        (InvariantBroken . show)
      let (sresp, counter') = runGenSym (mock sm smodel scmd) counter
      put QsmState {
          env = insertConcretes vars cvars env
        , smodel = transition sm smodel scmd sresp
        , counter = counter'
        , cmodel = transition sm cmodel ccmd cresp
        }

initialSt :: StateMachine model cmd m resp -> QsmState model
initialSt sm = QsmState {
    env = emptyEnvironment
  , smodel = initModel sm
  , counter = newCounter
  , cmodel = initModel sm
  }

runSemanticsViaCatch ::
    MonadCatch m =>
    m (resp Concrete) ->
    QsmM model cmd resp m (resp Concrete)
runSemanticsViaCatch sem = lift (try sem) >>= \case
    Left (ex :: SomeException) -> do
      let exStr = displayException ex
      logEvent $ Exception exStr
      finishWithReason $ ExceptionThrown exStr
    Right cresp -> pure cresp

runSequentialInternal ::
    forall model cmd m resp.
    ( MonadCatch m
    , Show (cmd Concrete)
    , Show (resp Concrete)
    , Rank2.Traversable cmd
    , Rank2.Foldable resp
    ) =>
    (m (resp Concrete) -> QsmM model cmd resp m (resp Concrete)) ->
    StateMachine model cmd m resp ->
    Commands cmd resp ->
    m (History cmd resp, model Concrete, Reason)
runSequentialInternal runSemantics sm cmds = do
    (hist, st, reason) <- runQsmM (initialSt sm) (Pid 0) $
      runCommandsSequentially runSemantics sm cmds
    pure (hist, st.cmodel, reason)


{- | Run a sequence of commands sequentially

Checks for:

 * precondition

 * exceptions

 * postcondition

 * invariant

 * mock semantics match
-}
runSequential ::
    forall model cmd m resp.
    ( MonadCatch m
    , Show (cmd Concrete)
    , Show (resp Concrete)
    , Rank2.Traversable cmd
    , Rank2.Foldable resp
    ) =>
    StateMachine model cmd m resp ->
    Commands cmd resp ->
    m (History cmd resp, model Concrete, Reason)
runSequential =
    runSequentialInternal runSemanticsViaCatch

runSequentialException ::
    forall model cmd m resp.
    ( MonadCatch m
    , Show (cmd Concrete)
    , Show (resp Concrete)
    , Rank2.Traversable cmd
    , Rank2.Foldable resp
    ) =>
    StateMachine model cmd m resp ->
    Commands cmd resp ->
    m (History cmd resp, model Concrete, Reason)
runSequentialException =
    runSequentialInternal runSemantics
  where
    runSemantics :: m (resp Concrete) -> QsmM model cmd resp m (resp Concrete)
    runSemantics sem = lift (try sem) >>= \case
      Left (ex :: SomeException) -> do
        logEvent $ Exception (displayException ex)
        finishWithReason Ok
      Right cresp -> pure cresp

{-------------------------------------------------------------------------------
  Parallel
-------------------------------------------------------------------------------}

runParallel ::
    forall model cmd resp m t.
    ( MonadCatch m
    , Show (cmd Concrete)
    , Show (resp Concrete)
    , Rank2.Traversable cmd
    , Rank2.Foldable resp
    , Traversable t
    ) =>
    StateMachine model cmd m resp ->
    ParallelCommandsF t cmd resp ->
    m (History cmd resp, model Concrete, Reason)
runParallel sm (ParallelCommands pref suff) = do
    (hist0, QsmState{env = env0, cmodel = cmodel0}, reason0) <-
      runQsmM (initialSt sm) (Pid 0) $
        runCommandsSequentially runSemanticsViaCatch sm pref
    if reason0 /= Ok
    then do
      pure (hist0, cmodel0, reason0)
    else do
      pure undefined

runParallelException :: ()
runParallelException = () -- TODO

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

data QsmState model = QsmState {
    env     :: !Environment
  , smodel  :: !(model Symbolic)
  , counter :: !Counter
  , cmodel  :: !(model Concrete)
  }

newtype QsmM model cmd resp m a = QsmM
  (ExceptT
     Reason
     (StateT
       (QsmState model)
       (WriterT
          (Seq (HistoryEvent cmd resp))
          m
       )
     )
     a
  )
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState (QsmState model)
    )

instance MonadTrans (QsmM model cmd resp) where
  lift = QsmM . lift . lift . lift

finishWithReason :: Monad m => Reason -> QsmM model cmd resp m a
finishWithReason = QsmM . throwError

checkLogic ::
  Monad m =>
  Value ->
  (Counterexample -> Reason) ->
  QsmM model cmd resp m ()
checkLogic val toReason = case val of
  VTrue     -> pure ()
  VFalse ce -> finishWithReason $ toReason ce

logEvent :: Monad m => HistoryEvent cmd resp -> QsmM model cmd resp m ()
logEvent = QsmM . tell . pure

runQsmM ::
  forall model cmd m resp a.
  Monad m =>
  QsmState model ->
  Pid ->
  QsmM model cmd resp m () ->
  m (History cmd resp, QsmState model, Reason)
runQsmM initialSt pid (QsmM action) = do
  ((ereason, st), evs) <-
    runWriterT $ runStateT (runExceptT action) initialSt
  pure (History $ (pid,) <$> toList evs, st, fromLeft Ok ereason)

{-------------------------------------------------------------------------------
  Vendored, maybe expose upstream?
-------------------------------------------------------------------------------}

getUsedConcrete :: Rank2.Foldable f => f Concrete -> [Dynamic]
getUsedConcrete = Rank2.foldMap (\(Concrete x) -> [toDyn x])

logicReason :: Reason -> Logic
logicReason Ok = Top
logicReason r  = Annotate (show r) Bot

mockCheck ::
    ( Foldable t
    , Rank2.Foldable resp
    , Show (cmd Concrete)
    , Show (t a)
    , Show (resp Concrete)
    ) =>
    cmd Concrete ->
    resp Concrete ->
    t a ->
    (Property, [Dynamic])
mockCheck ccmd cresp vars =
    let cvars = Rank2.foldMap (\(Concrete x) -> [toDyn x]) cresp
     in if length vars /= length cvars
            then
                let err = mockSemanticsMismatchError (ppShow ccmd) (ppShow vars) (ppShow cresp) (ppShow cvars)
                 in (counterexample ("Mock semantics mismatch: " <> err) $ property False, cvars)
            else (property True, cvars)

mockSemanticsMismatchError :: String -> String -> String -> String -> String
mockSemanticsMismatchError cmd svars cresp cvars =
    unlines
        [ ""
        , "Mismatch between `mock` and `semantics`."
        , ""
        , "The definition of `mock` for the command:"
        , ""
        , "    "
        , cmd
        , ""
        , "returns the following references:"
        , ""
        , "    "
        , svars
        , ""
        , "while the response from `semantics`:"
        , ""
        , "    "
        , cresp
        , ""
        , "returns the following references:"
        , ""
        , "    "
        , cvars
        , ""
        , "Continuing to execute commands at this point could result in scope"
        , "errors, because we might have commands that use references (returned"
        , "by `mock`) that are not available (returned by `semantics`)."
        , ""
        ]
