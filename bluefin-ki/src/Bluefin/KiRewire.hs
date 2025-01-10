module Bluefin.KiRewire where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.IO
import Bluefin.Internal (Eff (UnsafeMkEff), IOE (MkIOE), State (UnsafeMkState), unsafeUnEff)
import Bluefin.Ki
import Bluefin.State
import Data.IORef (newIORef, readIORef)
import GHC.IO.Handle (BufferMode (LineBuffering))
import GHC.IO.StdHandles (stdout)
import Ki (Thread)
import Ki qualified
import System.IO (hSetBuffering)

forkWithNewEff
    :: (e :> es)
    => Scope e
    -> Eff forkEs r
    -> Eff es (Thread r)
forkWithNewEff scope action = UnsafeMkEff $ Ki.fork (unsafeUnScope scope) do
    unsafeUnEff action

class ShareHandle a where
    forShare :: (e :> es) => a e -> Eff es (a sharedE)
    fromShared :: a sharedE -> (forall e. a e -> Eff (e :& es) r) -> Eff es r

instance ShareHandle IOE where
    forShare :: (e :> es) => IOE e -> Eff es (IOE sharedE)
    forShare _ = return MkIOE
    fromShared
        :: a sharedE
        -> (forall e. IOE e -> Eff (e :& es) r)
        -> Eff es r
    fromShared _ f = useImplIn f MkIOE

instance ShareHandle (State a) where
    forShare :: (e :> es) => State a e -> Eff es (State a sharedE)
    forShare (UnsafeMkState r) = return $ UnsafeMkState r
    fromShared
        :: (State a) sharedE
        -> (forall e. (State a) e -> Eff (e :& es) r)
        -> Eff es r
    fromShared (UnsafeMkState r) f = useImplIn f (UnsafeMkState r)

class CopyHandle a where
    forCopy :: forall copyE e es. (e :> es) => a e -> Eff es (a copyE)
    fromCopy :: a copyE -> (forall e. a e -> Eff (e :& es) r) -> Eff es r

instance CopyHandle IOE where
    forCopy :: (e :> es) => IOE e -> Eff es (IOE sharedE)
    forCopy _ = return MkIOE
    fromCopy
        :: a sharedE
        -> (forall e. IOE e -> Eff (e :& es) r)
        -> Eff es r
    fromCopy _ f = useImplIn f MkIOE

-- This is useless, we can just share variable.
instance CopyHandle (State a) where
    forCopy :: (e :> es) => State a e -> Eff es (State a sharedE)
    forCopy (UnsafeMkState r) = UnsafeMkEff do
        val <- readIORef r
        copy <- newIORef val
        return $ UnsafeMkState copy
    fromCopy
        :: (State a) sharedE
        -> (forall e. (State a) e -> Eff (e :& es) r)
        -> Eff es r
    fromCopy (UnsafeMkState r) f = do
        copy <- UnsafeMkEff do
            val <- readIORef r
            newIORef val
        useImplIn f (UnsafeMkState copy)

---------------------------------------

data MyEnv e = MkMyEnv
    { myState :: State Int e
    , myIO :: IOE e
    }

runMyEnv
    :: (eIO :> es)
    => IOE eIO
    -> (forall e. MyEnv e -> Eff (e :& es) r)
    -> Eff es r
runMyEnv io f = evalState 0 \s -> do
    useImplIn f (MkMyEnv (mapHandle s) (mapHandle io))

pokeEnv :: (e :> es) => MyEnv e -> Eff es ()
pokeEnv env = do
    modify (myState env) (+ 1)
    poked <- get (myState env)
    effIO (myIO env) do print $ "poked: " <> show poked

instance CopyHandle MyEnv where
    forCopy :: (e :> es) => MyEnv e -> Eff es (MyEnv copyE)
    forCopy (MkMyEnv s io) = do
        s' <- forCopy s
        io' <- forCopy io
        return (MkMyEnv s' io')
    fromCopy
        :: MyEnv sharedE
        -> (forall e. MyEnv e -> Eff (e :& es) r)
        -> Eff es r
    fromCopy (MkMyEnv s io) f = do
        fromCopy s \s' -> do
            fromCopy io \io' -> do
                useImplIn f (MkMyEnv (mapHandle s') (mapHandle io'))

instance ShareHandle MyEnv where
    forShare :: (e :> es) => MyEnv e -> Eff es (MyEnv copyE)
    forShare (MkMyEnv s io) = do
        s' <- forShare s
        io' <- forShare io
        return (MkMyEnv s' io')
    fromShared
        :: MyEnv sharedE
        -> (forall e. MyEnv e -> Eff (e :& es) r)
        -> Eff es r
    fromShared (MkMyEnv s io) f = do
        fromShared s \s' -> do
            fromShared io \io' -> do
                useImplIn f (MkMyEnv (mapHandle s') (mapHandle io'))

----------------------------------------
useShare :: IO Int
useShare =
    hSetBuffering stdout LineBuffering *> runEff \io -> do
        runScope io \scope -> do
            evalState 5 \s -> do
                runMyEnv io \env -> do
                    sharedIO <- forShare io
                    sharedS <- forShare s
                    sharedEnv <- forShare env
                    _ <- forkWithNewEff scope do
                        fromShared sharedS \threadS -> do
                            x <- get threadS
                            put threadS (x + 1)
                        fromShared sharedEnv \threadEnv -> do
                            pokeEnv threadEnv
                        fromShared sharedIO \threadIO -> do
                            effIO threadIO $ print "thread 1"
                    _ <- forkWithNewEff scope do
                        fromShared sharedS \threadS -> do
                            x <- get threadS
                            put threadS (x * 2)
                        fromShared sharedEnv \threadEnv -> do
                            pokeEnv threadEnv
                        fromShared sharedIO \threadIO -> do
                            effIO threadIO $ print "thread 2"
                    awaitAll scope
                    get s

useCopy :: IO Int
useCopy =
    hSetBuffering stdout LineBuffering *> runEff \io -> do
        runScope io \scope -> do
            evalState 5 \s -> do
                runMyEnv io \env -> do
                    copyS <- forCopy s
                    copyEnv <- forCopy env
                    _ <- forkWithNewEff scope do
                        fromCopy copyS \threadS -> do
                            x <- get threadS
                            put threadS (x + 1)
                        fromCopy copyEnv \threadEnv -> do
                            pokeEnv threadEnv
                    _ <- forkWithNewEff scope do
                        fromCopy copyS \threadS -> do
                            x <- get threadS
                            put threadS (x * 2)
                        fromCopy copyEnv \threadEnv -> do
                            pokeEnv threadEnv
                    awaitAll scope
                    get s
