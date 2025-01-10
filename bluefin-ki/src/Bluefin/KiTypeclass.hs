module Bluefin.KiTypeclass where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.IO
import Bluefin.Internal (Eff (UnsafeMkEff), IOE (MkIOE), unsafeUnEff)
import Bluefin.Ki
import Bluefin.State
import Ki (Thread)
import Ki qualified

forkWithNewEff
    :: (e :> es)
    => Scope e
    -> Eff forkEs r
    -> Eff es (Thread r)
forkWithNewEff scope action = UnsafeMkEff $ Ki.fork (unsafeUnScope scope) do
    unsafeUnEff action

class Shared a b where
    freeze :: (e :> es) => a e -> Eff es b
    unFreeze :: b -> (forall e. a e -> Eff (e :& es) r) -> Eff es r

-- Meh.
data FrozenIOE = MkFrozenIOE

instance Shared IOE FrozenIOE where
    freeze :: (e :> es) => IOE e -> Eff es FrozenIOE
    freeze _ = return MkFrozenIOE
    unFreeze :: FrozenIOE -> (forall e. IOE e -> Eff (e :& es) r) -> Eff es r
    unFreeze _ f = useImplIn f MkIOE

useWithNewEff :: IO Int
useWithNewEff = runEff \io -> do
    runScope io \scope -> do
        evalState 5 \s -> do
            (ios :: FrozenIOE) <- freeze io
            _ <- forkWithNewEff scope do
                unFreeze ios \io' -> do
                    effIO io' $ print "thread 1"
                evalState (1 :: Integer) \s1 -> do
                    put s1 3
            _ <- forkWithNewEff scope do
                unFreeze ios \io' -> do
                    effIO io' $ print "thread 2"
                evalState (2 :: Integer) \s1 -> do
                    put s1 4
            awaitAll scope
            get s
