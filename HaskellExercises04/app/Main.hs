{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HaskellExercises04.Main
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises04 and a main function that runs each tests and prints the results
-}
module Main where

import Prelude hiding (zip,take,drop)
import qualified Exercises04 as E4

-- | Main Program
main :: IO ()
main = putStrLn "Compiled Exercises04 successfully"
