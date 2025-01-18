module Bluefin.Parallel.Exception where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.Exception
import Bluefin.IO
import Bluefin.Internal (
    Eff (UnsafeMkEff),
    returnEarly,
    unsafeProvideIO,
    unsafeUnEff,
    withEarlyReturn,
 )
import Bluefin.State
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar)
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified
import Control.Monad (forM_, forever)
import Data.Coerce (coerce)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Unique (Unique)
import Data.Unique qualified as Unique
import Ki qualified
import Unsafe.Coerce (unsafeCoerce)

data PureScope (scopeE :: Effects) (e :: Effects) = UnsafeMkPureScope
    { scopeKiScope :: Ki.Scope
    , scopeEvents :: STM.TChan (Maybe ScopeEvent)
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

-- Left throwing exception.
-- Right awaiting for thread with id `TrId`.
data ThreadState = ThreadState TrId (Either ParExResult TrId)
    deriving (Show)

data ScopeEvent
    = EvAwaiting TrId TrId
    | EvDoneAwaiting TrId TrId
    | EvException TrId ParExResult
    deriving (Show)

-- This is an exception, but it should never leave `runParallel`.
-- TODO Maybe we could switch to EarlyReturn/checked exceptions.
data ParExResult where
    ParExResult :: e -> TrId -> ParExResult

instance Show ParExResult where
    show (ParExResult _ trId) = "<ParExResult trId:" <> show trId <> ">"

instance Control.Exception.Exception ParExResult

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

                -- \| Terminate `parallelMonitor`
                STM.atomically do STM.writeTChan eventChan Nothing
                STM.atomically (Ki.awaitAll s) -- FIXME do we need it?
                return r

        r <- effIO io $ flip Control.Exception.tryJust parRun $ \case
            ParExResult @ee e trId ->
                if trId == mainId
                    then Just (unsafeCoerce @ee e)
                    else Nothing

        case r of
            Right res -> pure res
            Left e -> throw exception e

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
                ParExResult @exT e trId ->
                    if trId == childId
                        then Just (ParExResult e trId)
                        else Nothing
            case r of
                Right res -> return res
                Left childEx -> do
                    STM.atomically do STM.writeTChan chan (Just $ EvException childId childEx)
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
        STM.atomically do
            STM.writeTChan chan (Just $ threadId `EvAwaiting` otherId)
        r <- STM.atomically (Ki.await t)
        STM.atomically do
            STM.writeTChan chan (Just $ threadId `EvDoneAwaiting` otherId)
        return r

pureThrow
    :: (sE :> exSE, scopeE :> es, exE :> es)
    => PureScope sE scopeE
    -> ParException exT exSE exE
    -> exT
    -> Eff es ()
pureThrow (UnsafeMkPureScope _ _ trId _) _ exV = UnsafeMkEff do
    Control.Exception.throwIO (ParExResult exV trId)

parallelMonitor :: STM.TChan (Maybe ScopeEvent) -> TrId -> IO ()
parallelMonitor chan mainId = runEff \io -> do
    evalState [] \s -> do
        withEarlyReturn \ret -> do
            forever do
                mbEvent <- effIO io do STM.atomically (STM.readTChan chan)
                case mbEvent of
                    Nothing -> do
                        returnEarly ret ()
                    Just event -> do
                        advanceState mainId s event >>= \case
                            Nothing -> pure ()
                            Just (ParExResult exV _) ->
                                effIO io do
                                    Control.Exception.throwIO
                                        (ParExResult exV mainId)

{- | TODO we operate under the assumption that there
is only one element in the list for each threadId,
so this should be a map.
-}
advanceState
    :: (e :> es)
    => TrId
    -> State [ThreadState] e
    -> ScopeEvent
    -> Eff es (Maybe ParExResult)
advanceState mainId state = \cases
    (originId `EvDoneAwaiting` _) -> do
        -- This event doesn't seem necessary, it just keeps
        -- our state tidy. Maybe `EvThreadEnded` event would be
        -- better for this.
        removeFrom state originId
        return Nothing
    (originId `EvAwaiting` otherId) -> do
        removeFrom state originId
        isThrowing state otherId >>= \case
            Nothing -> do
                addTo state (ThreadState originId (Right otherId))
            Just ex -> do
                addTo state (ThreadState originId (Left ex))
                propagateException state originId ex
        isThrowing state mainId
    (EvException originId ex) -> do
        removeFrom state originId
        addTo state (ThreadState originId (Left ex))
        propagateException state originId ex
        isThrowing state mainId
  where
    propagateException
        :: (e :> es)
        => State [ThreadState] e
        -> TrId
        -> ParExResult
        -> Eff es ()
    propagateException s sourceId ex = do
        awaitingIds <- awaitingFor s sourceId
        forM_ awaitingIds \awaitingId -> do
            removeFrom s awaitingId
            addTo s $ ThreadState awaitingId (Left ex)
            propagateException s awaitingId ex

    isThread :: TrId -> ThreadState -> Bool
    isThread trId (ThreadState someId _) = trId == someId

    removeFrom :: (e :> es) => State [ThreadState] e -> TrId -> Eff es ()
    removeFrom s trId = modify s $ filter (not . isThread trId)

    addTo :: (e :> es) => State [ThreadState] e -> ThreadState -> Eff es ()
    addTo s t = modify s (t :)

    isThrowing
        :: (e :> es)
        => State [ThreadState] e
        -> TrId
        -> Eff es (Maybe ParExResult)
    isThrowing s trId =
        listToMaybe . mapMaybe getException . filter (isThread trId)
            <$> get s
      where
        getException (ThreadState _ (Left ex)) = Just ex
        getException _ = Nothing

    awaitingFor :: (e :> es) => State [ThreadState] e -> TrId -> Eff es [TrId]
    awaitingFor s targetId =
        map (\(ThreadState tid _) -> tid)
            . filter isAwaiting
            <$> get s
      where
        isAwaiting (ThreadState _ (Right awaitId)) = awaitId == targetId
        isAwaiting _ = False
