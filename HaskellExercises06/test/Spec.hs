{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HaskellExercises06.Spec
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises06 and a main function that runs each tests and prints the results
-}
module Main where

import qualified Exercises06 as E6
import Test.QuickCheck (quickCheck
                       ,quickCheckResult
                       ,quickCheckWithResult
                       ,stdArgs
                       ,maxSuccess
                       ,Result(Success)
                       ,within
                       ,Testable)
import Test.Hspec
import Test.QuickCheck.Property (property)

-------------------------------------------------------------------------------------------
-- * QuickCheck Tests

-- | Existential type wrapper for QuickCheck propositions, allows @propList@ to essentially
--   act as a heterogeneous list that can hold any quickcheck propositions of any type
data QuickProp = forall prop . Testable prop =>
                 QuickProp { quickPropName :: String
                           , quickPropMark :: Int
                           , quickPropFunc :: prop
                           }

-- | Boolean implication
(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y
infixr 4 ==>

-- | QuickCheck proposition for testing Exercises06.zipWith
zipWithProp :: [Int] -> [Int] -> Bool
zipWithProp xs ys = E6.zipWith (+) xs ys == zipWith (+) xs ys

-- | QuickCheck proposition for testing Exercises06.foldr
foldrProp :: Int -> [Int] -> Bool
foldrProp x xs = E6.foldr (-) x xs == foldr (-) x xs

-- | QuickCheck proposition for testing Exercises06.foldl
foldlProp :: Int -> [Int] -> Bool
foldlProp x xs = E6.foldl (-) x xs == foldl (-) x xs

-- | QuickCheck proposition for testing Exercises06.concat
concatProp :: [[Int]] -> Bool
concatProp xss = E6.concat xss == concat xss

-- | QuickCheck proposition for testing Exercises06.concatMap
concatMapProp :: [Int] -> Bool
concatMapProp xs = E6.concatMap (replicate 2) xs == concatMap (replicate 2) xs

-- | QuickCheck proposition for testing Exercises06.lookup
lookupProp :: Int -> [(Int,Char)] -> Bool
lookupProp k ds = E6.lookup k ds == lookup k ds


-------------------------------------------------------------------------------------------
-- * Run Tests
main :: IO ()
main = hspec $ do
  describe "zipWith" $ do
    it "should correspond to Prelude.zipWith" $ property $ zipWithProp
  describe "foldr" $ do
    it "should correspond to Prelude.foldr" $ property $ foldrProp
  describe "foldl" $ do
    it "should correspond to Prelude.foldl" $ property $ foldlProp
  describe "concat" $ do
    it "should correspond to Prelude.concat" $ property $ concatProp
  describe "concatMap (replicate 2)" $ do
    it "should correspond to Prelude.concatMap (replicate 2)" $ property $ concatMapProp
  describe "lookup" $ do
    it "should correspond to Prelude.lookup" $ property $ lookupProp
