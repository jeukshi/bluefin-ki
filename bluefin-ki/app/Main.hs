module Main where

import Bluefin.KiExceptionExamples (runForks)
import Bluefin.KiPureExamples
import Control.Monad.Par (runPar)
import Criterion.Main

main :: IO ()
main = do
    print runForks

{-let r = streamPar2
let r2 = streamPar2
print $ "r: " <> show r
print $ "r2: " <> show r2
-}

main' :: IO ()
main' = do
    let f (x :: Integer) = sum [x .. 9_999_999]
    defaultMain
        [ bgroup
            "fib"
            [ bench "pure" $ whnf (runPureFib 29) 30
            , bench "monad-par" $ whnf (runParFib 29) 30
            , bench "runBfFib" $ whnf (runBfFib 29) 30
            , bench "runBfFibWithGet" $ whnf (runBfFibWithGet 29) 30
            ]
        , bgroup
            "map"
            [ bench "pure" $ nf (map f) (replicate 4 1)
            , bench "parMap'book" $ nf (runPar . parMap'book f) (replicate 4 1)
            , bench "parMapBf" $ nf (parMapBf f) (replicate 4 1)
            ]
        ]
