module Bluefin.Parallel.Stream where

import Bluefin.Compound
import Bluefin.EarlyReturn (returnEarly, withEarlyReturn)
import Bluefin.Eff
import Bluefin.IO (effIO)
import Bluefin.Internal (Eff (UnsafeMkEff), State (UnsafeMkState), unsafeProvideIO, unsafeUnEff)
import Bluefin.Parallel
import Bluefin.State
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM qualified as STM
import Control.DeepSeq (NFData, deepseq)
import Control.Exception.Base (finally)
import Control.Monad (forever, unless)
import Data.Foldable (for_)
import Data.IORef (newIORef)
import Data.Vector.Mutable qualified as V
import Ki qualified

debugDelay :: Int -> Eff es ()
debugDelay = UnsafeMkEff . threadDelay

debugPrint :: (Show a) => a -> Eff es ()
debugPrint = UnsafeMkEff . print . show

newtype Producer a e = UnsafeMkProducer (TChan (Maybe a))

instance Handle (Producer a) where
    mapHandle :: (e :> es) => Producer a e -> Producer a es
    mapHandle (UnsafeMkProducer scope) = UnsafeMkProducer scope

data Consumer a (scopeE :: Effects) (consumerE :: Effects) = UnsafeMkConsumer
    { consumerSValues :: V.MVector V.RealWorld a
    , consumerSMax :: TVar (Either Int Int)
    , consumerCursor :: State Int consumerE
    }

instance Handle (Consumer a scopeE) where
    mapHandle :: (e :> es) => Consumer a scopeE e -> Consumer a scopeE es
    mapHandle (UnsafeMkConsumer a b c) = UnsafeMkConsumer a b (mapHandle c)

getConsumer
    :: (sE :> es, pscE :> psE)
    => PureScope psE sE
    -> Consumer a pscE consumerE
    -> (forall e e2. Consumer a e2 e -> Eff (e :& es) r)
    -> Eff es r
getConsumer _ (UnsafeMkConsumer values sMax _) f = do
    evalState 0 \s ->
        useImplIn f (UnsafeMkConsumer values sMax (mapHandle s))

-- TODO remove Show
pureProducer
    :: (e :> es)
    => (Show a)
    => PureScope sE e
    -> (Producer a pE -> Eff (e :& pE :& forkEs) r)
    -> Eff es (Consumer a sE es, PureThread r sE es)
pureProducer (UnsafeMkPureScope scope) action = do
    (pt, values, sMax) <- UnsafeMkEff do
        chan <- STM.atomically STM.newTChan
        values <- V.unsafeNew 1000 -- TODO
        sMax <- STM.atomically do STM.newTVar (Right 0)
        _ <- Ki.fork scope do
            producerMonitor chan values sMax
        pt <- Ki.fork scope do
            unsafeUnEff (action (UnsafeMkProducer chan))
                `finally` STM.atomically do STM.writeTChan chan Nothing
        return (pt, values, sMax)
    UnsafeMkEff do
        s <- fmap UnsafeMkState (newIORef 0)
        return (UnsafeMkConsumer values sMax s, UnsafeMkPureThread pt)
  where
    producerMonitor
        :: (Show a)
        => TChan (Maybe a)
        -> V.MVector V.RealWorld a
        -> TVar (Either Int Int)
        -> IO ()
    producerMonitor chan values sMax = runEff \io -> do
        evalState (0 :: Int) \received -> do
            withEarlyReturn \ret -> do
                forever do
                    newVal <- effIO io $ STM.atomically do
                        STM.readTChan chan
                    case newVal of
                        Nothing -> do
                            rec <- get received
                            effIO io do
                                STM.atomically do STM.writeTVar sMax (Left rec)
                            returnEarly ret ()
                        (Just val) -> do
                            ix <- get received
                            modify received (+ 1)
                            rec <- get received
                            effIO io do
                                V.unsafeWrite values ix val
                                STM.atomically do STM.writeTVar sMax (Right rec)

yield
    :: (e :> es)
    => (NFData a)
    => Producer a e
    -> a
    -> Eff es ()
yield (UnsafeMkProducer chan) v = UnsafeMkEff do
    STM.atomically do STM.writeTChan chan (Just (v `deepseq` v))

forEach
    :: (e :> es)
    => Consumer a forkEs e
    -> (a -> Eff es ())
    -> Eff es ()
forEach (UnsafeMkConsumer values sMax cursor) f = do
    unsafeProvideIO \io -> do
        withEarlyReturn \ret -> do
            forever do
                cursorVal <- get cursor
                (noToRead, continue) <- effIO io $ atomically $ do
                    readTVar sMax >>= \case
                        (Left currentMax) ->
                            return (currentMax - cursorVal, False)
                        (Right currentMax) ->
                            if cursorVal == currentMax
                                then STM.retry
                                else return (currentMax - cursorVal, True)
                for_ (replicate noToRead ()) \_ -> do
                    modify cursor (+ 1)
                    curr <- get cursor
                    val <- effIO io do V.read values (curr - 1)
                    useImpl $ f val
                unless continue do
                    returnEarly ret ()
