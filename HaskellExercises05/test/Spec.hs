{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HaskellExercises05.Spec
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises05 and a main function that runs each tests and prints the results
-}
module Main where

import qualified Exercises05 as E5
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
import Data.List (sort)

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

-- | QuickCheck proposition for testing Exercises05.split
splitProp :: [Int] -> Bool
splitProp xs =
  let
    (left,right) = E5.split xs
  in (left ++ right) == xs
    && length left == (length xs `div` 2)

-- | QuickCheck proposition for testing Exercises05.merge
mergeProp :: [Int] -> [Int] -> Bool
mergeProp xs ys =
  let
    xs' = sort xs
    ys' = sort ys
    ms  = E5.merge xs' ys'
  in ms == sort (xs++ys)

-- | QuickCheck proposition for testing Exercises05.mergeSort
mergeSortProp :: [Int] -> Bool
mergeSortProp xs = E5.mergeSort xs == sort xs

-- | QuickCheck proposition for testing Exercises05.sortProp
sortPropProp :: [Int] -> Bool
sortPropProp xs =
  let
    isSorted = xs == sort xs
  in E5.sortProp xs == isSorted

-- | QuickCheck proposition for testing Exercises05.replicate
replicateProp0 :: Int -> Int -> Bool
replicateProp0 n x =
  let
    repls = E5.replicate n x
  in and [ r == x | r <- repls ]

replicateProp1 :: Int -> Int -> Bool
replicateProp1 n x =
  let
    repls = E5.replicate n x
  in (length repls == n) || (n < 0 && length repls == 0)

-- | QuickCheck proposition for testing Exercises05.!!
indexProp :: [Int] -> Int -> Bool
indexProp xs n = (n >= 0 && n < length xs) ==> (xs E5.!! n == xs !! n)


-- | QuickCheck proposition for testing Exercises05.elem
elemProp :: Int -> [Int] -> Bool
elemProp e xs = ( E5.elem e xs ) == ( or [ e == x | x <- xs ] )

-------------------------------------------------------------------------------------------
-- * Run Tests
main :: IO ()
main = hspec $ do
  describe "split" $ do
    it "(left,right) == split xs should recombine to one list, and left should be length xs `div` 2" $ property $ splitProp
  describe "merge" $ do
    it "for input (xs,ys), merge (sort xs) (sort ys) should have the same result as sort (xs ++ ys)" $ property $ mergeProp
  describe "mergeSortProp" $ do
    it "mergeSort xs should result in a sorted list as output" $ property $ mergeSortProp
  describe "sortProp" $ do
    it "should only return true if the input it sorted" $ property $ sortPropProp
  describe "replicate" $ do
    it "every element returned by replicate should be the same element" $ property $ replicateProp0
    it "length (replicate n x) == n" $ property $ replicateProp1
  describe "!!" $ do
    it "should correspond to Prelude.!!" $ property $ indexProp
  describe "elem e xs" $ do
    it "should only return true when e is in the list xs" $ property $ elemProp
