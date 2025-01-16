module Bluefin.Parallel.Exception where

import Bluefin.Compound
import Bluefin.EarlyReturn
import Bluefin.Eff
import Bluefin.Exception
import Bluefin.IO
import Bluefin.Internal (Eff (UnsafeMkEff), unsafeProvideIO, unsafeUnEff, withEarlyReturn)
import Bluefin.State
import Bluefin.StateSource (newState, withStateSource)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar)
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified
import Control.Monad (forever, unless)
import Data.Coerce (coerce)
import Data.List (find)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Tree
import Data.Unique (Unique)
import Data.Unique qualified as Unique
import GHC.Conc (ThreadId)
import Ki qualified
import Unsafe.Coerce (unsafeCoerce)

data PureScope (scopeE :: Effects) (e :: Effects) = UnsafeMkPureScope
    { scopeKiScope :: Ki.Scope
    , scopeEvents :: STM.TChan ScopeEvent
    , scopeCurrentThread :: TrId
    , scopeForeverEmpty :: MVar ()
    }

instance Handle (PureScope scopeE) where
    mapHandle :: (e :> es) => PureScope scopeE e -> PureScope scopeE es
    mapHandle (UnsafeMkPureScope scope chan i m) = UnsafeMkPureScope scope chan i m

data PureThread r (scopeE :: Effects) (e :: Effects) = UnsafeMkPureThread
    { threadKiThread :: Ki.Thread r
    , threadId :: TrId
    }

instance Handle (PureThread r scopeE) where
    mapHandle :: (e :> es) => PureThread r scopeE e -> PureThread r scopeE es
    mapHandle (UnsafeMkPureThread t i) = UnsafeMkPureThread t i

newtype ParException ex (scopeE :: Effects) (e :: Effects)
    = UnsafeMkParException (Proxy ex)

newtype TrId = UnsafeMkTrId Unique
    deriving (Eq)

newTrId :: IO TrId
newTrId = coerce Unique.newUnique

instance Show TrId where
    show = ("TrId" <>) . show . Unique.hashUnique . coerce

data ThreadInfo = ThreadInfo TrId ThreadState
    deriving (Show)

data ThreadState
    = Awaiting TrId
    | Throwing BfParThreadPureException
    deriving (Show)

data ScopeEvent
    = EvMainFinished
    | EvAwaiting TrId TrId
    | EvDoneAwaiting TrId TrId
    | EvException TrId BfParThreadPureException
    deriving (Show)

data BfParThreadPureException where
    BfParThreadPureException :: e -> TrId -> BfParThreadPureException

instance Show BfParThreadPureException where
    show (BfParThreadPureException _ trId) = "<PAREX trId:" <> show trId <> ">"

instance Control.Exception.Exception BfParThreadPureException

runParallelEx
    :: forall exT scopeE exE es r parEs
     . (exE :> es)
    => Exception exT exE
    -> ( forall e e2
          . PureScope scopeE e
         -> ParException exT scopeE e2
         -> Eff (e :& e2 :& parEs) r
       )
    -> Eff es r
runParallelEx exception action = do
    unsafeProvideIO \io -> do
        mainId <- effIO io newTrId
        let parRun = Ki.scoped \s -> do
                eventChan <- STM.atomically STM.newTChan
                emptyForever <- newEmptyMVar
                _ <- Ki.fork s (parallelMonitor eventChan mainId)
                r <- unsafeUnEff do
                    action
                        (UnsafeMkPureScope s eventChan mainId emptyForever)
                        (UnsafeMkParException (Proxy @exT))

                STM.atomically do STM.writeTChan eventChan EvMainFinished
                STM.atomically (Ki.awaitAll s) -- FIXME do we need it?
                return r

        r <- effIO io $ flip Control.Exception.tryJust parRun $ \case
            BfParThreadPureException @ee e trId ->
                if trId == mainId
                    then Just (unsafeCoerce @ee e)
                    else Nothing

        case r of
            Right res -> pure res
            Left e -> throw exception e
  where
    parallelMonitor :: STM.TChan ScopeEvent -> TrId -> IO ()
    parallelMonitor chan mainId = runEff \io -> do
        evalState [] \s -> do
            withEarlyReturn \ret -> do
                forever do
                    event <- effIO io do STM.atomically (STM.readTChan chan)
                    ss <- get s
                    effIO io do putStrLn $ show event <> "  STATE: "
                    case event of
                        EvMainFinished -> do
                            effIO io do putStrLn "FINISHEDDDD"
                            returnEarly ret ()
                        (someId `EvDoneAwaiting` otherId) -> do
                            modify s $
                                filter
                                    ( \(ThreadInfo tid state) ->
                                        case state of
                                            Awaiting awaitId -> not (tid == someId && awaitId == otherId)
                                            _ -> True
                                    )
                        (someId `EvAwaiting` otherId) -> do
                            threads <- get s
                            case isThreadThrowing otherId threads of
                                Just ex -> do
                                    modify s $ \ts ->
                                        ThreadInfo someId (Throwing ex)
                                            : filter (\(ThreadInfo tid _) -> tid /= someId) ts
                                    propagateException someId ex s
                                Nothing -> do
                                    modify s $ \ts ->
                                        ThreadInfo someId (Awaiting otherId)
                                            : filter (\(ThreadInfo tid _) -> tid /= someId) ts

                            threads' <- get s
                            effIO io do print threads'
                            case checkMainThrowing mainId threads' of
                                Just ex -> case ex of
                                    BfParThreadPureException exV _ ->
                                        effIO io $
                                            Control.Exception.throwIO
                                                (BfParThreadPureException exV mainId)
                                Nothing -> pure ()
                        (EvException threadId threadEx) -> do
                            modify s $ \ts ->
                                ThreadInfo threadId (Throwing threadEx)
                                    : filter (\(ThreadInfo tid _) -> tid /= threadId) ts
                            propagateException threadId threadEx s
                            threads <- get s
                            effIO io do print threads
                            case checkMainThrowing mainId threads of
                                Just ex -> case ex of
                                    BfParThreadPureException exV _ ->
                                        effIO io $
                                            Control.Exception.throwIO
                                                (BfParThreadPureException exV mainId)
                                Nothing -> pure ()

checkMainThrowing :: TrId -> [ThreadInfo] -> Maybe BfParThreadPureException
checkMainThrowing mainId threads =
    case find (\(ThreadInfo tid _) -> tid == mainId) threads of
        Just (ThreadInfo _ (Throwing ex)) -> Just ex
        _ -> Nothing

findAwaiting :: TrId -> [ThreadInfo] -> [TrId]
findAwaiting targetId = map (\(ThreadInfo tid _) -> tid) . filter isAwaiting
  where
    isAwaiting (ThreadInfo _ (Awaiting awaitId)) = awaitId == targetId
    isAwaiting _ = False

isThreadThrowing :: TrId -> [ThreadInfo] -> Maybe BfParThreadPureException
isThreadThrowing tid = listToMaybe . mapMaybe getException . filter matchId
  where
    matchId (ThreadInfo id_ _) = id_ == tid
    getException (ThreadInfo _ (Throwing ex)) = Just ex
    getException _ = Nothing

propagateException
    :: forall e es
     . (e :> es)
    => TrId
    -> BfParThreadPureException
    -> State [ThreadInfo] e
    -> Eff es ()
propagateException sourceId ex s = do
    threads <- get s
    let awaitingIds = findAwaiting sourceId threads
    unless (null awaitingIds) $ do
        modify s $ \ts ->
            [ThreadInfo tid (Throwing ex) | tid <- awaitingIds]
                ++ filter
                    ( \(ThreadInfo tid _) ->
                        tid `notElem` awaitingIds
                    )
                    ts
        mapM_ (\tid -> propagateException tid ex s) awaitingIds

pureFork
    :: (e :> es)
    => PureScope sE e
    -> (forall e2. PureScope sE e2 -> Eff (e2 :& forkEs) r)
    -> Eff es (PureThread r sE es)
pureFork (UnsafeMkPureScope scope chan _ emptyForever) action = do
    (t, childId) <- UnsafeMkEff do
        childId <- newTrId
        t <- Ki.fork scope do
            unsafeUnEff (action (UnsafeMkPureScope scope chan childId emptyForever))
        return (t, childId)
    return $ UnsafeMkPureThread t childId

pureForkEx
    :: (e :> es, sE :> exSE)
    => PureScope sE e
    -> ParException exT exSE exE
    -> ( forall e2 e3
          . PureScope sE e2
         -> ParException exT exSE e3
         -> Eff (e2 :& e3 :& forkEs) r
       )
    -> Eff es (PureThread r sE es)
pureForkEx (UnsafeMkPureScope scope chan _ emptyForever) ex action = do
    (t, childId) <- UnsafeMkEff do
        childId <- newTrId
        t <- Ki.fork scope do
            let f = unsafeUnEff (action (UnsafeMkPureScope scope chan childId emptyForever) ex)
            r <- flip Control.Exception.tryJust f $ \case
                BfParThreadPureException @exT e trId ->
                    if trId == childId
                        then Just (BfParThreadPureException e trId)
                        else Nothing
            case r of
                Right res -> return res
                Left childEx -> do
                    STM.atomically do STM.writeTChan chan (EvException childId childEx)
                    readMVar emptyForever
                    error "MVar wasn't empty uuu" -- TODO we need some BluefinAssertionException
        return (t, childId)
    return $ UnsafeMkPureThread t childId

pureGet
    :: (scopeE :> es, sE :> tsE)
    => PureScope sE scopeE
    -> PureThread a tsE e
    -> Eff es a
pureGet (UnsafeMkPureScope _ chan threadId _) (UnsafeMkPureThread t otherId) =
    UnsafeMkEff do
        -- TODO check if thread is pure
        STM.atomically do STM.writeTChan chan (threadId `EvAwaiting` otherId)
        r <- STM.atomically (Ki.await t)
        STM.atomically do STM.writeTChan chan (threadId `EvDoneAwaiting` otherId)
        return r

pureThrow
    :: (sE :> exSE, scopeE :> es, exE :> es)
    => PureScope sE scopeE
    -> ParException exT exSE exE
    -> exT
    -> Eff es ()
pureThrow (UnsafeMkPureScope _ _ trId _) _ exV = UnsafeMkEff do
    Control.Exception.throwIO (BfParThreadPureException exV trId)
