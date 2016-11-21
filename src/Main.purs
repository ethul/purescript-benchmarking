module Main where

import Prelude hiding (max)

import Control.Monad.Eff (Eff)

import Data.Array ((..), uncons)
import Data.Foldable (foldl)
import Data.Identity (Identity(..))
import Data.Maybe (maybe)
import Data.Traversable (sequence)

import Test.QuickCheck.Gen (vectorOf)

import Benchotron.Core (Benchmark, BenchEffects, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)

import FreeAp9ccd7db as FreeApOld

import FreeAp71acf67 as FreeApNew

max :: Int
max = 100

max2 :: Int
max2 = 10

benchRetractFreeAp :: Benchmark
benchRetractFreeAp = mkBenchmark
  { slug: "retractFreeAp"
  , title: "retractFreeAp (sequence [ FreeAp f a, ..., FreeAp f z ])"
  , sizes: (1..20) <#> (_ * max)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> { old: _
               , new: _
               } <$> vectorOf n (pure (FreeApOld.liftFreeAp (Identity unit)))
                 <*> vectorOf n (pure (FreeApNew.liftFreeAp (Identity unit)))
  , functions: [ benchFn "FreeAp Old @ 9ccd7db" (\a -> FreeApOld.retractFreeAp (sequence a.old))
               , benchFn "FreeAp New @ 71acf67" (\a -> FreeApNew.retractFreeAp (sequence a.new))
               ]
  }

type A = { old :: Array (FreeApOld.FreeAp Identity Unit)
         , new :: Array (FreeApNew.FreeAp Identity Unit)
         }

benchFoldFreeAp :: Benchmark
benchFoldFreeAp = mkBenchmark
  { slug: "foldFreeAp"
  , title: "foldFreeAp (a -> a) (k <$> FreeAp f a <* FreeAp f b <* ... <* FreeAp f z)"
  , sizes: (1..20) <#> (_ * max2)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> { old: _
               , new: _
               } <$> vectorOf n (pure (FreeApOld.liftFreeAp (Identity unit)))
                 <*> vectorOf n (pure (FreeApNew.liftFreeAp (Identity unit)))
  , functions: [ benchFn "FreeAp Old @ 9ccd7db" (\(a :: A) ->
                   maybe (pure unit)
                         (\{head, tail} -> FreeApOld.foldFreeAp (\a' -> a') (id <$> (foldl (\x y -> x <* y) head tail)))
                         (uncons a.old))
               , benchFn "FreeAp New @ 71acf67" (\(a :: A) ->
                   maybe (pure unit)
                         (\{head, tail} -> FreeApNew.foldFreeAp (\a' -> a') (id <$> (foldl (\x y -> x <* y) head tail)))
                         (uncons a.new))
               ]
  }

main :: forall eff. Eff (BenchEffects eff) Unit
main = runSuite [ benchRetractFreeAp
                , benchFoldFreeAp
                ]
