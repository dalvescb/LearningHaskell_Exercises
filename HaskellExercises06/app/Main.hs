{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HaskellExercises06.Main
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises06 and a main function that runs each tests and prints the results
-}
module Main where

import Prelude hiding (zipWith,foldr,foldl,concat,concatMap,lookup)
import qualified Exercises06 as E6

-- | Main Program
main :: IO ()
main = putStrLn "Successfully compiled Exercises06"
