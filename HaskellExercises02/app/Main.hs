{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HaskellExercises02.Main
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises02 and a main function that runs each tests and prints the results
-}
module Main where

import Prelude hiding ((||),(&&),abs)
import qualified Exercises02 as E2

-- | Main Program
main :: IO ()
main = do putStrLn "Compiled Exercises02 successfully"
