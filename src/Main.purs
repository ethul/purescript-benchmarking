module Main where

import Prelude hiding (max)

import Control.Monad.Eff (Eff)

import Data.Array ((..), uncons)
import Data.Foldable (foldl)
import Data.Identity (Identity(..))
import Data.Maybe (maybe)
import Data.String (toLower)
import Data.Traversable (sequence)

import Test.QuickCheck.Gen (vectorOf)

import Benchotron.Core (Benchmark, BenchEffects, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)

import FreeAp9ccd7db as FreeApOld

import FreeAp71acf67 as FreeApNew

import FreeApDay as FreeApDay

trials :: Int
trials = 1

benchRetractFreeAp :: Int -> String -> Benchmark
benchRetractFreeAp size label = mkBenchmark
  { slug: "retractFreeAp-71acf67-9ccd7db-day-" <> toLower label
  , title: "retractFreeAp (sequence [ FreeAp f a, ..., FreeAp f z ]) - " <> label
  , sizes: (1..20) <#> (_ * size)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: trials
  , gen: \n -> { old: _
               , new: _
               , day: _
               } <$> vectorOf n (pure (FreeApOld.liftFreeAp (Identity unit)))
                 <*> vectorOf n (pure (FreeApNew.liftFreeAp (Identity unit)))
                 <*> vectorOf n (pure (FreeApDay.liftFreeAp (Identity unit)))
  , functions: [ benchFn "FreeAp Old @ 9ccd7db" (\a -> FreeApOld.retractFreeAp (sequence a.old))
               , benchFn "FreeAp New @ 71acf67" (\a -> FreeApNew.retractFreeAp (sequence a.new))
               , benchFn "FreeAp Day" (\a -> FreeApDay.retractFreeAp (sequence a.day))
               ]
  }

type A = { old :: Array (FreeApOld.FreeAp Identity Unit)
         , new :: Array (FreeApNew.FreeAp Identity Unit)
         , day :: Array (FreeApDay.FreeAp Identity Unit)
         }

benchFoldFreeAp :: Int -> String -> Benchmark
benchFoldFreeAp size label = mkBenchmark
  { slug: "foldFreeAp-71acf67-9ccd7db-day-" <> toLower label
  , title: "foldFreeAp (a -> a) (k <$> FreeAp f a <* FreeAp f b <* ... <* FreeAp f z) - " <> label
  , sizes: (1..12) <#> (_ * size)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: trials
  , gen: \n -> { old: _
               , new: _
               , day: _
               } <$> vectorOf n (pure (FreeApOld.liftFreeAp (Identity unit)))
                 <*> vectorOf n (pure (FreeApNew.liftFreeAp (Identity unit)))
                 <*> vectorOf n (pure (FreeApDay.liftFreeAp (Identity unit)))
  , functions: [ benchFn "FreeAp Old @ 9ccd7db" (\(a :: A) ->
                   maybe (pure unit)
                         (\{head, tail} -> FreeApOld.foldFreeAp (\a' -> a') (id <$> (foldl (\x y -> x <* y) head tail)))
                         (uncons a.old))
               , benchFn "FreeAp New @ 71acf67" (\(a :: A) ->
                   maybe (pure unit)
                         (\{head, tail} -> FreeApNew.foldFreeAp (\a' -> a') (id <$> (foldl (\x y -> x <* y) head tail)))
                         (uncons a.new))
               , benchFn "FreeAp Day" (\(a :: A) ->
                   maybe (pure unit)
                         (\{head, tail} -> FreeApDay.foldFreeAp (\a' -> a') (id <$> (foldl (\x y -> x <* y) head tail)))
                         (uncons a.day))
               ]
  }

main :: forall eff. Eff (BenchEffects eff) Unit
main = runSuite [ benchRetractFreeAp 2 "Small"
                , benchFoldFreeAp 1 "Small"
                , benchRetractFreeAp 100 "Large"
                , benchFoldFreeAp 10 "Large"
                ]
