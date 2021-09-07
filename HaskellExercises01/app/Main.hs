{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HaskellExercises01.Main
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
   Default main program
-}
module Main where

import Prelude hiding (last,init,(!!))

import qualified Exercises01 as E1

-- | Main Program
main :: IO ()
main = putStrLn "HaskellExercises01 compiled successfully"
