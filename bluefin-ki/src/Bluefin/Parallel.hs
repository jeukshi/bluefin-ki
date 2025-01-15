{-# LANGUAGE NoMonoLocalBinds #-}

module Bluefin.Parallel where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.Internal (Eff (UnsafeMkEff), unsafeUnEff)
import GHC.Conc qualified as STM
import Ki qualified

newtype PureScope (scopeE :: Effects) (e :: Effects) = UnsafeMkPureScope
    {unsafeUnPureScope :: Ki.Scope}

instance Handle (PureScope scopeE) where
    mapHandle :: (e :> es) => PureScope scopeE e -> PureScope scopeE es
    mapHandle (UnsafeMkPureScope scope) = UnsafeMkPureScope scope

newtype PureThread r (scopeE :: Effects) (e :: Effects) = UnsafeMkPureThread
    {unsafeUnPureThread :: Ki.Thread r}

instance Handle (PureThread r scopeE) where
    mapHandle :: (e :> es) => PureThread r scopeE e -> PureThread r scopeE es
    mapHandle (UnsafeMkPureThread t) = UnsafeMkPureThread t

runParallel
    :: (forall e. PureScope scopeE e -> Eff (e :& es) r)
    -> Eff es r
runParallel action = do
    UnsafeMkEff $ Ki.scoped \s -> do
        r <- unsafeUnEff $ action (UnsafeMkPureScope s)
        STM.atomically (Ki.awaitAll s) -- FIXME do we need it?
        return r

pureFork
    :: (e :> es)
    => PureScope sE e
    -> Eff (e :& forkEs) r
    -> Eff es (PureThread r sE es)
pureFork (UnsafeMkPureScope scope) action = do
    t <- UnsafeMkEff $ Ki.fork scope do
        unsafeUnEff action
    return $ UnsafeMkPureThread t

pureGet
    :: (scopeE :> es, sE :> tsE)
    => PureScope sE scopeE
    -> PureThread a tsE e
    -> Eff es a
pureGet _ (UnsafeMkPureThread t) =
    UnsafeMkEff (STM.atomically (Ki.await t))
