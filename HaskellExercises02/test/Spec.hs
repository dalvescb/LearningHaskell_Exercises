{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HaskellExercises02.Spec
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises01 and a main function that runs each tests and prints the results
-}
module Main where

import qualified Exercises02 as E2
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

-- | QuickCheck proposition for testing Exercises02.||
orProp :: Bool -> Bool -> Bool
orProp a b = (a E2.|| b) == (a || b)

-- | QuickCheck proposition for testing Exercises02.&&
andProp :: Bool -> Bool -> Bool
andProp a b = (a E2.&& b) == (a && b)

-- | QuickCheck proposition for testing Exercises02.==>
impliesProp :: Bool -> Bool -> Bool
impliesProp a b = (a E2.==> b ) == ((not a) || b)

-- | QuickCheck proposition for testing Exercises02.abs
absProp :: Int -> Bool
absProp x = (E2.abs x) == (abs x)

-- | QuickCheck proposition for testing Exercises02.=.
floatEqualsProp :: Float -> Float -> Bool
floatEqualsProp x y = not (abs (x-y) <= 1e-4) || (x E2.=. y)

-- | QuickCheck proposition for testing Exercises02.stack
stackProp :: [Int] -> Bool
stackProp (x:xs) = E2.stack (x:xs) == (xs ++ [x])
stackProp []     = True

-- | QuickCheck proposition for testing Exercises02.halves
halvesProp :: [Int] -> Bool
halvesProp xs = and [ y*2 <= x | (x,y) <- (zip xs $ E2.halves xs) ]

-------------------------------------------------------------------------------------------
-- * Run Tests
main :: IO ()
main = hspec $ do
  describe "||" $ do
    it "should correspond to Prelude.||" $ property $ orProp
  describe "&&" $ do
    it "should correspond to Prelude.&&" $ property $ andProp
  describe "==>" $ do
    it "a ==> b should correspond to (not a) || b" $ property $ impliesProp
  describe "abs" $ do
    it "should correspond to Prelude.abs" $ property $ absProp
  describe "=." $ do
    it "should be correct when the inputs are with 1e-4 tolerance of eachother" $ property $ floatEqualsProp
  describe "stack" $ do
    it "stack (x:xs) should put x to the back of xs" $ property $ stackProp
  describe "halves" $ do
    it "multiplying each element of halves xs by 2 should be <= the corresponding element" $ property $ halvesProp
