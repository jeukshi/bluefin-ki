module Bluefin.KiPureExamples where

import Bluefin.Eff (runPureEff)
import Bluefin.Parallel
import Bluefin.Parallel.Stream
import Bluefin.State
import Control.DeepSeq (NFData)
import Control.Monad.Par qualified as Par
import Data.Foldable (for_)

fib :: Int -> Int
fib = \cases
    0 -> 0
    1 -> 1
    n -> fib (n - 1) + fib (n - 2)

runPureFib :: Int -> Int -> Int
runPureFib n m = do
    let a = fib n
    let b = fib m
    a + b

runParFib :: Int -> Int -> Int
runParFib n m = Par.runPar do
    i <- Par.new
    j <- Par.new
    Par.fork (Par.put i (fib n))
    Par.fork (Par.put j (fib m))
    a <- Par.get i
    b <- Par.get j
    pure $ a + b

runBfFib :: Int -> Int -> Int
runBfFib n m = runPureEff do
    runParallel \par -> do
        at <- pureFork par do
            let !r = fib n
            pure r
        bt <- pureFork par do
            let !r = fib m
            pure r
        a <- pureGet par at
        b <- pureGet par bt
        pure $ a + b

runBfFibWithGet :: Int -> Int -> Int
runBfFibWithGet n m = runPureEff do
    -- pointless, just testing `pureGet`
    runParallel \par -> do
        runParallel \_ -> do
            at <- pureFork par do
                let !r = fib n
                pure r
            bt <- pureFork par do
                let !r = fib m
                pure r
            ct <- pureFork par do
                a <- pureGet par at
                b <- pureGet par bt
                pure $ a + b
            pureGet par ct

parMap'book :: (NFData b) => (a -> b) -> [a] -> Par.Par [b]
parMap'book f as = do
    ibs <- traverse (Par.spawn . return . f) as
    traverse Par.get ibs

parMapBf :: (a -> b) -> [a] -> [b]
parMapBf f xs = runPureEff do
    runParallel \par -> do
        let tf x = pureFork par do
                let !x' = f x -- deepseq?
                return x'
        ts <- traverse tf xs
        traverse (pureGet par) ts

runGetTwice :: Int -> Int
runGetTwice n = runPureEff do
    -- pointless, just testing `pureGet`
    runParallel \par -> do
        runParallel \_ -> do
            at <- pureFork par do
                let !r = fib n
                pure r
            bt <- pureFork par do
                a <- pureGet par at
                b <- pureGet par at
                pure $ a + b
            pureGet par bt

streamPar :: Int
streamPar = runPureEff do
    runParallel \par -> do
        runParallel \_ -> do
            (c, t1) <- pureProducer par \y -> do
                let (l :: [Int]) = [0 .. 9]
                for_ l \e -> do
                    debugDelay 1_000_000
                    yield y e
                return 999
            getConsumer par c \cm -> do
                forEach cm \el -> do
                    debugPrint $ "main: read from stream: " <> show el
                forEach cm \el -> do
                    debugPrint $ "main: read from stream: " <> show el
                forEach cm \el -> do
                    debugPrint $ "main: read from stream: " <> show el
            pureGet par t1

streamPar2 :: Int
streamPar2 = runPureEff do
    runParallel \par -> do
        (c, t1) <- pureProducer par \y -> do
            let (l :: [Int]) = [0 .. 9]
            for_ l \e -> do
                debugDelay 1_000_000
                yield y e
            return 999
        _ <- pureFork par do
            getConsumer par c \ct -> do
                forEach ct \t1El ->
                    debugPrint $ "t1: read from stream: " <> show t1El
        getConsumer par c \cl -> do
            forEach cl \el -> do
                debugPrint $ "main: read from stream: " <> show el
        pureGet par t1

streamSum :: [Int] -> Int
streamSum l = runPureEff do
    runParallel \par -> do
        (c, _) <- pureProducer par \y -> do
            for_ l \e -> do
                debugDelay 1_000_000
                yield y e
        t1 <- pureFork par do
            evalState 0 \s -> do
                getConsumer par c \ct -> do
                    forEach ct \t1El ->
                        modify s (+ t1El)
                get s
        pureGet par t1

streamSumTwice :: [Int] -> Int
streamSumTwice l = runPureEff do
    runParallel \par -> do
        (c, _) <- pureProducer par \y -> do
            for_ l \e -> do
                debugDelay 1_000_000
                yield y e
        t1 <- pureFork par do
            evalState 0 \s -> do
                getConsumer par c \ct -> do
                    forEach ct \tEl ->
                        modify s (+ tEl)
                get s
        t2 <- pureFork par do
            runParallel \_ -> do
                debugDelay 3_000_000 -- Start later.
                evalState 0 \s -> do
                    getConsumer par c \ct -> do
                        forEach ct \tEl ->
                            modify s (+ tEl)
                    get s
        r1 <- pureGet par t1
        r2 <- pureGet par t2
        return $ r1 + r2

streamSumTwiceInOneT :: [Int] -> Int
streamSumTwiceInOneT l = runPureEff do
    runParallel \par -> do
        (c, _) <- pureProducer par \y -> do
            for_ l \e -> do
                debugDelay 1_000_000
                yield y e
        t1 <- pureFork par do
            evalState 0 \s -> do
                getConsumer par c \ct -> do
                    forEach ct \tEl ->
                        modify s (+ tEl)
                    forEach ct \tEl ->
                        -- does nothing
                        modify s (+ tEl)
                getConsumer par c \ct -> do
                    forEach ct \tEl ->
                        modify s (+ tEl)
                    forEach ct \tEl ->
                        -- does nothing
                        modify s (+ tEl)
                get s
        pureGet par t1
