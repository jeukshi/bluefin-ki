module Main where

import Bluefin.KiExceptionExamples
import Bluefin.KiPureExamples
import Control.Monad.Par (runPar)
import Criterion.Main

main :: IO ()
main = do
    let l = [3, 2, 1, 0]
    print $ validateList l
    print $ validateListPar l

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
