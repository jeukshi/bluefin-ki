module Bluefin.Ki where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.IO
import Bluefin.Internal (Eff (UnsafeMkEff), unsafeUnEff)
import Bluefin.Reader
import Bluefin.State
import Control.Monad (forever)
import GHC.Conc (threadDelay)
import GHC.Conc qualified as STM
import GHC.IO.Handle (BufferMode (LineBuffering))
import GHC.IO.Handle.FD (stdout)
import Ki (Thread)
import Ki qualified
import System.IO (hSetBuffering)

newtype Scope (e :: Effects) = UnsafeMkScope {unsafeUnScope :: Ki.Scope}

instance Handle Scope where
    mapHandle :: (e :> es) => Scope e -> Scope es
    mapHandle (UnsafeMkScope scope) = UnsafeMkScope scope

runScope
    :: (eIO :> es)
    => IOE eIO
    -> (forall e. Scope e -> Eff (e :& es) r)
    -> Eff es r
runScope _ action = do
    UnsafeMkEff $ Ki.scoped \s -> do
        unsafeUnEff $ action (UnsafeMkScope s)

fork
    :: (e :> es)
    => Scope e
    -> Eff es r
    -> Eff es (Thread r)
fork scope action = UnsafeMkEff $ Ki.fork (unsafeUnScope scope) do
    unsafeUnEff action

awaitAll
    :: (e :> es)
    => Scope e
    -> Eff es ()
awaitAll scope = UnsafeMkEff $ STM.atomically do
    Ki.awaitAll (unsafeUnScope scope)

useStateConcurrently :: IO Int
useStateConcurrently = runEff \io -> do
    runScope io \scope -> do
        evalState 5 \s -> do
            _ <- fork scope do
                effIO io do threadDelay 500
                ls <- get s
                put s (ls + 1)
            _ <- fork scope do
                effIO io do threadDelay 1000
                ls <- get s
                put s (ls * 2)
            awaitAll scope
            get s

-- BAD
useReaderConcurrently :: IO ()
useReaderConcurrently =
    hSetBuffering stdout LineBuffering >> runEff \io -> do
        runScope io \scope -> do
            runReader "none" \r -> do
                _ <- fork scope do
                    initVal <- ask r
                    effIO io do print $ "thread 1 init..." <> show initVal
                    local r (const "hello from thread 1") do
                        effIO io do threadDelay 500
                        currVal <- ask r
                        effIO io do print $ "thread 1 after sleep..." <> show currVal
                _ <- fork scope do
                    initVal <- ask r
                    effIO io do print $ "thread 2 init..." <> show initVal
                    local r (const "hello from thread 2") do
                        effIO io do threadDelay 500
                        currVal <- ask r
                        effIO io do print $ "thread 2 after sleep..." <> show currVal
                awaitAll scope

notSoGreatEscape :: IO ()
notSoGreatEscape = do
    hSetBuffering stdout LineBuffering
    res <- runEff \io -> do
        runScope io \scope -> do
            _ <- fork scope do
                effIO io $ forever do
                    print "still alive"
                    threadDelay 2_000
            effIO io $ threadDelay 4_000
            return "eff done"
    print res
    threadDelay 6_000
