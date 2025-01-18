module Bluefin.KiExceptionExamples where

import Bluefin.Eff
import Bluefin.Exception
import Bluefin.Parallel.Exception
import Control.Monad (when)

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

validateList :: [Int] -> Either String [Int]
validateList xs = runPureEff do
    try \e -> do
        when (1 `elem` xs) do throw e "1"
        when (2 `elem` xs) do throw e "2"
        when (3 `elem` xs) do throw e "3"
        return xs

validateListPar :: [Int] -> Either String [Int]
validateListPar xs = runPureEff do
    try \(e :: Exception String e) -> do
        runParallelEx e \par ex -> do
            t2 <- pureForkEx par ex \p1 ex1 -> do
                when (2 `elem` xs) do pureThrow p1 ex1 "2"
            t3 <- pureForkEx par ex \p1 ex1 -> do
                when (3 `elem` xs) do pureThrow p1 ex1 "3"
            when (1 `elem` xs) do pureThrow par ex "1"
            pureGet par t2
            pureGet par t3
            return xs
