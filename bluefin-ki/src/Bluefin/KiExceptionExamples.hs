module Bluefin.KiExceptionExamples where

import Bluefin.Eff
import Bluefin.Exception
import Bluefin.Parallel.Exception

runForks :: Either String ()
runForks = runPureEff do
    try \(e :: Exception String e) -> do
        runParallelEx e \par ex -> do
            t1 <- pureForkEx par ex \p1 ex1 -> do
                pureThrow p1 ex1 "t1p1"
                _ <- pureFork p1 \_ -> do
                    pure ()
                _ <- pureFork p1 \_ -> do
                    pure ()
                pure ()
            t2 <- pureForkEx par ex \p1 ex1 -> do
                pureGet p1 t1
                _ <- pureForkEx p1 ex1 \p2 ex2 -> do
                    _ <- pureForkEx p2 ex2 \p3 ex3 -> do
                        pureThrow p3 ex3 "t2p3"
                        pure ()
                    pure ()
                pure ()
            pureGet par t2
            -- pureGet par t1
            pure ()
