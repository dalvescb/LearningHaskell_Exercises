{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HaskellExercises05.Main
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises05 and a main function that runs each tests and prints the results
-}
module Main where

import Prelude hiding (take,drop,replicate,(!!),elem,and,or)
import qualified Exercises05 as E5

-- | Main Program
main :: IO ()
main = putStrLn "Compiled Exercises05 successfully"
