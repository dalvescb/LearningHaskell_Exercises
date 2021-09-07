{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HaskellExercises03.Spec
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises03 and a main function that runs each tests and prints the results
-}
module Main where

import qualified Exercises03 as E3
import Test.QuickCheck (quickCheck
                       ,quickCheckResult
                       ,quickCheckWithResult
                       ,stdArgs
                       ,maxSuccess
                       ,Result(Success)
                       ,within
                       ,Testable, Positive (Positive))
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

-- | Boolean implication
(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y
infixr 4 ==>

-- | QuickCheck proposition for testing Exercises03.fib
fibProp :: Int -> Bool
fibProp n = (n >= 2 && n <= 10) ==> ((E3.fib n - E3.fib (n-1)) == E3.fib (n-2))

-- | Flatten a Tree into a list
flatten :: E3.Tree a -> [a]
flatten (E3.Leaf x) = [x]
flatten (E3.Node x t1 t2) = flatten t1 ++ [x] ++ flatten t2

-- | QuickCheck proposition for testing Exercises03.exTree
exTreeProp :: Bool
exTreeProp = flatten E3.exTree == ['a','b','c','d','e','f','g']

-- | Convert a Int to a Nat type, will throw an error if negative
int2nat :: Int -> E3.Nat
int2nat x
  | x < 0     = error "int2nat called on negative int"
  | x == 0    = E3.Zero
  | otherwise = E3.Succ (int2nat (x-1))

-- | Convert a Nat to a Int type
nat2int :: E3.Nat -> Int
nat2int E3.Zero = 0
nat2int (E3.Succ n) = 1 + nat2int n

-- | Arbitrary instance for Nat (simply generates an arbitary positive Int and converts it to Nat)
instance Arbitrary (E3.Nat) where
  arbitrary = do
    Positive x <- arbitrary
    return $ int2nat x

-- | QuickCheck proposition for testing Exercises03.mult
multProp :: E3.Nat -> E3.Nat -> Bool
multProp n0 n1 =
  let
    m0 = nat2int n0 * nat2int n1
    m1 = nat2int $ E3.mult n0 n1
  in m0 == m1

-- | Arbitrary instance for Tree
instance Arbitrary a => Arbitrary (E3.Tree a) where
  arbitrary =
    let
      genTree n
        | n <= 0 = do v <- arbitrary
                      return $ E3.Leaf v
        | otherwise = do (Positive n0) <- arbitrary
                         t0 <- genTree (n `div` (n0+1)) -- generate an arbitrary sized left tree
                         (Positive n1) <- arbitrary
                         t1 <- genTree (n `div` (n1+1)) -- generate an arbitrary sized right tree
                         v <- arbitrary
                         return $ E3.Node v t0 t1
      in sized genTree

-- | QuickCheck proposition for testing Exercises03.isSearchTree
isSearchTreeProp :: E3.Tree Int -> Bool
isSearchTreeProp ts =
  let
    isSorted xs = and . map (\(x,y) -> x <= y) . zip xs $ tail xs
    isSearch    = isSorted $ flatten ts
  in isSearch == E3.isSearchTree ts

-- | QuickCheck proposition for testing Exercises03.factors
factorsProp :: Int -> Bool
factorsProp n =
  let
    fs = E3.factors n
  in and [ n `mod` f == 0 | f <- fs]

-- | QuickCheck proposition for testing Exercises03.pivot
pivotProp :: Int -> [Int] -> Bool
pivotProp v xs =
  let
    (lefts,rights) = E3.pivot v xs
    allLessThan    = and [ l <= v | l <- lefts ]
    allGreaterThan = and [ r >  v | r <- rights ]
  in allLessThan && allGreaterThan

-------------------------------------------------------------------------------------------
-- * Run Tests
main :: IO ()
main = hspec $ do
  describe "fib" $ do
    it "fib n - fib (n-1) == fib (n-2)" $ property $ fibProp
  describe "exTree" $ do
    it "flatten exTree should equal ['a','b','c',...]" $ property $ exTreeProp
  describe "mult" $ do
    it "multipying the integer conversions should yield the same result" $ property $ multProp
  describe "isSearchTree" $ do
    it "isSearchTree should only return true when the flattened tree is sorted" $ property $ isSearchTreeProp
  describe "factors" $ do
    it "all elements of factors should evenly divise n" $ property $ factorsProp
  describe "pivot" $ do
    it "(left,right) = pivot v xs should have all elements of left <= v and right > v" $ property $ pivotProp
