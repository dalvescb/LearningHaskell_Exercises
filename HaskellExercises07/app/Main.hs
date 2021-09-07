{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HaskellExercises07.Main
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises07 and a main function that runs each tests and prints the results
-}
module Main where

import qualified Exercises07 as E7

import Prelude hiding (curry,fmap)

-- | Main Program
main :: IO ()
main = putStrLn "Compiled Exercises07 Succesfully"
