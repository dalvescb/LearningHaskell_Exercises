{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HaskellExercises04.Spec
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises04 and a main function that runs each tests and prints the results
-}
module Main where

import qualified Exercises04 as E4
import Test.QuickCheck (quickCheck
                       ,quickCheckResult
                       ,quickCheckWithResult
                       ,stdArgs
                       ,maxSuccess
                       ,Result(Success)
                       ,within
                       ,Testable
                       ,Positive (..))
import Test.Hspec
import Test.QuickCheck.Property (property)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (sized)

-------------------------------------------------------------------------------------------
-- * QuickCheck Tests

-- | Existential type wrapper for QuickCheck propositions, allows @propList@ to essentially
--   act as a heterogeneous list that can hold any quickcheck propositions of any type
data QuickProp = forall prop . Testable prop =>
                 QuickProp { quickPropName :: String
                           , quickPropMark :: Int
                           , quickPropFunc :: prop
                           }

-- | Arbitrary instance for Tree
instance Arbitrary a => Arbitrary (E4.Tree a) where
  arbitrary =
    let
      genTree n
        | n <= 0 = return $ E4.Empty
        | otherwise = do (Positive n0) <- arbitrary
                         t0 <- genTree (n `div` (n0+1)) -- generate an arbitrary sized left tree
                         (Positive n1) <- arbitrary
                         t1 <- genTree (n `div` (n1+1)) -- generate an arbitrary sized right tree
                         v <- arbitrary
                         return $ E4.Node v t0 t1
      in sized genTree

-- | Convert a [a] to a List a
list2MyList :: [a] -> E4.List a
list2MyList [] = E4.Nil
list2MyList (x:xs) = E4.Cons x (list2MyList xs)

-- | Convert a List a to a [a]
myList2list :: E4.List a -> [a]
myList2list E4.Nil = []
myList2list (E4.Cons x xs) = x : (myList2list xs)

-- | Flatten a tree into a list
flatten :: E4.Tree a -> [a]
flatten E4.Empty = []
flatten (E4.Node v t0 t1) = flatten t0 ++ [v] ++ flatten t1

-- | Arbitrary instance for List
instance Arbitrary a => Arbitrary (E4.List a) where
  arbitrary = do xs <- arbitrary
                 return $ list2MyList xs

-- | Boolean implication
(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y
infixr 4 ==>

-- | QuickCheck proposition for testing Exercises04.zip
zipProp :: [Int] -> [Int] -> Bool
zipProp xs ys = E4.zip xs ys == zip xs ys


-- | QuickCheck proposition for testing Exercises04.mapWithIndex
mapWithIndexProp0 :: [Int] -> Bool
mapWithIndexProp0 xs = E4.mapWithIndex fst xs == [0..length xs-1]

mapWithIndexProp1 :: [Int] -> Bool
mapWithIndexProp1 xs = E4.mapWithIndex snd xs == xs

-- | QuickCheck proposition for testing Exercises04.mySum
mySumProp :: E4.List Int -> Bool
mySumProp xs = sum (myList2list xs) == E4.mySum xs

-- | QuickCheck proposition for testing Exercises04.+++
listConProp :: E4.List Int -> E4.List Int -> Bool
listConProp xs ys =
  let
    xs' = myList2list xs
    ys' = myList2list ys
  in (xs' ++ ys') == myList2list (xs E4.+++ ys)

-- | QuickCheck proposition for testing Exercises04.mySum
myReverseProp :: E4.List Int -> Bool
myReverseProp xs = (reverse $ myList2list xs) == (myList2list $ E4.myReverse xs)

-- | QuickCheck proposition for testing Exercises04.treeSum
treeSumProp :: E4.Tree Int -> Bool
treeSumProp tree = (E4.treeSum tree) == (sum $ flatten tree)

-- | QuickCheck proposition for testing Exercises04.treeHeight
treeHeightProp :: E4.Tree Int -> Bool
treeHeightProp tree =
  case tree of
    E4.Empty -> (E4.treeHeight tree) == 0
    E4.Node v t0 t1 -> (E4.treeHeight tree) == (1 + max (E4.treeHeight  t0) (E4.treeHeight t1))

-- | QuickCheck proposition for testing Exercises04.take and Exercises04.drop
takeDropProp :: Int -> [Int] -> Bool
takeDropProp n xs = (E4.take n xs ++ E4.drop n xs) == xs

-------------------------------------------------------------------------------------------
-- * Run Tests
main :: IO ()
main = hspec $ do
  describe "zip" $ do
    it "should correspond to Prelude.zip" $ property $ zipProp
  describe "mapWithIndex" $ do
    it "mapWithIndex fst xs == [0..length xs-1]" $ property $ mapWithIndexProp0
    it "mapWithIndex snd xs == xs" $ property $ mapWithIndexProp1
  describe "mySum" $ do
    it "should correspond to Prelude.sum (when converting to built-in list)" $ property $ mySumProp
  describe "+++" $ do
    it "should correspond to Prelude.++ (when converting to built-in lists)" $ property $ listConProp
  describe "myReverse" $ do
    it "should correspond to Prelude.reverse (when converting to built-in lists)" $ property $ myReverseProp
  describe "treeSum" $ do
    it "should sum every element in the tree" $ property $ treeSumProp
  describe "treeHeight" $ do
    it "should compute the maximum height of the tree" $ property $ treeHeightProp
  describe "takeDropProp" $ do
    it "take and drop should combine to create the original list" $ property $ takeDropProp
