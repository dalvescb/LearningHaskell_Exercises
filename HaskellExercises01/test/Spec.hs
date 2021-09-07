{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HaskellExercises01.Spec
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises01 and a main function that runs each tests and prints the results
-}
module Main where

import qualified Exercises01 as E1
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

-- | QuickCheck proposition for testing Exercises01.last
lastProp :: [Int] -> Bool
lastProp xs = null xs || (E1.last xs == Prelude.last xs)

-- | QuickCheck proposition for testing Exercises01.init
initProp :: [Int] -> Bool
initProp xs = null xs || (E1.init xs == Prelude.init xs)

-- | QuickCheck proposition for testing Exercises01.!!
indexProp :: [Int] -> Int -> Bool
indexProp xs i =
  let
    cond = not (null xs) && 0 <= i && i < length xs
  in cond ==> (xs E1.!! i == xs !! i)

-- | QuickCheck proposition for testing Exercises01.firstHalf and Exercises01.lastHalf
halvesProp0 :: [Int] -> Bool
halvesProp0 xs =
  let
    half0 = E1.firstHalf xs
    half1 = E1.lastHalf xs
  in half0 ++ half1 == xs

halvesProp1 :: [Int] -> Bool
halvesProp1 xs =
  let
    half0 = E1.firstHalf xs
    half1 = E1.lastHalf xs
  in (length xs `div` 2) == length half0

-- | QuickCheck proposition for testing Exercises01.inners
innersProp :: [Int] -> Bool
innersProp xs
  | length xs < 3 = E1.inners xs == []
  | otherwise     = ([head xs]  ++ E1.inners xs ++ [last xs]) == xs

-- | QuickCheck proposition for testing Exercises01.distance
distanceProp :: (Float,Float) -> Bool
distanceProp (p,q) =
  let
    tol = 1e-3
    d   = E1.distance (p,q) (0,0)
    m   = sqrt (p^2 + q^2)
  in abs (d - m) <= tol -- the distance from the origin should be the same as the origin (within fp error)

-- | QuickCheck proposition for testing Exercises01.nthRoot
nthRootProp :: Float -> Int -> Bool
nthRootProp x n =
  let
    tol = 1e-3
    root = E1.nthRoot x n
    cond = (x > 0) && (n > 0)
  in cond ==> (( abs (product [root | _ <- [1..n]]  - x) ) <= tol )

-------------------------------------------------------------------------------------------
-- * Run Tests
main :: IO ()
main = hspec $ do
  describe "last" $ do
    it "should correspond to Prelude.last" $ property $ lastProp
  describe "init" $ do
    it "should correspond to Prelude.init" $ property $ initProp
  describe "!!" $ do
    it "should correspond to Prelude.!!" $ property $ indexProp
  describe "firstHalf and secondHalf" $ do
    it "firstHalf xs ++ lastHalf xs == xs" $ property $ halvesProp0
    it "length (firstHalf xs) == length xs `div` 2" $ property $ halvesProp1
  describe "inners xs" $ do
    it "if length xs <= 2 then inners xs == [], otherwise ([head xs]  ++ inners xs ++ [last xs]) == xs" $ property $ innersProp
  describe "distance (p,q)" $ do
    it "distance (p,q) (0,0,) ~= sqrt (p^2 + q^2)" $ property $ distanceProp
  describe "nthRoot" $ do
    it "(nthRoot x n) multiplied n times should approx equal x" $ property $ nthRootProp
