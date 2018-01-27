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

import FreeApStackSafe as FreeApStackSafe

import FreeApDay as FreeApDay

trials :: Int
trials = 1

benchRetractFreeAp :: Boolean -> Int -> String -> Benchmark
benchRetractFreeAp shouldRunOld size label = mkBenchmark
  { slug: "retractFreeAp-71acf67" <> (if shouldRunOld then "-9ccd7db" else "") <> "-day-safe-" <> toLower label
  , title: "retractFreeAp (sequence [ FreeAp f a, ..., FreeAp f z ]) - " <> label
  , sizes: (1..20) <#> (_ * size)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: trials
  , gen: \n -> { old: _
               , new: _
               , day: _
               , safe: _
               } <$> vectorOf n (pure (FreeApOld.liftFreeAp (Identity unit)))
                 <*> vectorOf n (pure (FreeApNew.liftFreeAp (Identity unit)))
                 <*> vectorOf n (pure (FreeApDay.liftFreeAp (Identity unit)))
                 <*> vectorOf n (pure (FreeApStackSafe.liftFreeAp (Identity unit)))
  , functions: [ benchFn "FreeAp New @ 71acf67" (\a -> FreeApNew.retractFreeAp (sequence a.new))
               , benchFn "FreeAp Day" (\a -> FreeApDay.retractFreeAp (sequence a.day))
               , benchFn "FreeAp stacksafe" (\a -> FreeApStackSafe.retractFreeAp (sequence a.safe))
               ]  <> oldBench
  }
  where
  oldBench =
    if shouldRunOld then
      [ benchFn "FreeAp Old @ 9ccd7db" (\a -> FreeApOld.retractFreeAp (sequence a.old)) ]
    else []

type A = { old :: Array (FreeApOld.FreeAp Identity Unit)
         , new :: Array (FreeApNew.FreeAp Identity Unit)
         , day :: Array (FreeApDay.FreeAp Identity Unit)
         , safe :: Array (FreeApStackSafe.FreeAp Identity Unit)
         }

benchFoldFreeAp :: Boolean -> Boolean -> Int -> String -> Benchmark
benchFoldFreeAp shouldRunOld shouldRunDay size label = mkBenchmark
  { slug: "foldFreeAp-71acf67" <> (if shouldRunOld then "-9ccd7db" else "") <> (if shouldRunDay then "-day" else "")<> "-safe-" <> toLower label
  , title: "foldFreeAp (a -> a) (k <$> FreeAp f a <* FreeAp f b <* ... <* FreeAp f z) - " <> label
  , sizes: (1..12) <#> (_ * size)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: trials
  , gen: \n -> { old: _
               , new: _
               , day: _
               , safe: _
               } <$> vectorOf n (pure (FreeApOld.liftFreeAp (Identity unit)))
                 <*> vectorOf n (pure (FreeApNew.liftFreeAp (Identity unit)))
                 <*> vectorOf n (pure (FreeApDay.liftFreeAp (Identity unit)))
                 <*> vectorOf n (pure (FreeApStackSafe.liftFreeAp (Identity unit)))
  , functions: [ benchFn "FreeAp New @ 71acf67" (\(a :: A) ->
                   maybe (pure unit)
                         (\{head, tail} -> FreeApNew.foldFreeAp (\a' -> a') (id <$> (foldl (\x y -> x <* y) head tail)))
                         (uncons a.new))
               , benchFn "FreeAp stacksafe" (\(a :: A) ->
                   maybe (pure unit)
                         (\{head, tail} -> FreeApStackSafe.foldFreeAp (\a' -> a') (id <$> (foldl (\x y -> x <* y) head tail)))
                         (uncons a.safe))
               ] <> dayBench <> oldBench
  }
  where
  dayBench = 
    if shouldRunDay then 
      [ benchFn "FreeAp Day" (\(a :: A) ->
          maybe (pure unit)
                (\{head, tail} -> FreeApDay.foldFreeAp (\a' -> a') (id <$> (foldl (\x y -> x <* y) head tail)))
                (uncons a.day))
      ]
    else []
  oldBench = 
    if shouldRunOld then 
      [ benchFn "FreeAp Old @ 9ccd7db" (\(a :: A) ->
          maybe (pure unit)
                (\{head, tail} -> FreeApOld.foldFreeAp (\a' -> a') (id <$> (foldl (\x y -> x <* y) head tail)))
                (uncons a.old))
      ]
    else []

main :: forall eff. Eff (BenchEffects eff) Unit
main = runSuite [ benchRetractFreeAp true 2 "Small"
                , benchFoldFreeAp true true 1 "Small"
                , benchRetractFreeAp true 100 "Large"
                , benchRetractFreeAp false 125 "Large"
                , benchFoldFreeAp true true 10 "Large"
                , benchFoldFreeAp false true 180 "Large"
                , benchFoldFreeAp false false 250 "Large"
                ]
