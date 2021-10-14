{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HaskellExercises08.Spec
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises08 and a main function that runs each tests and prints the results
-}
module Main where

import qualified Exercises08 as E8
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

-- | QuickCheck porposition of testing Exercises08.sNatToInt and Exercises08.intToSNat
natConvertProp :: Integer -> Bool
natConvertProp x = (E8.sNatToInt $ E8.intToSNat x) == x

-- | QuickCheck proposition for testing Exercises08.addSNat
addSNatProp :: Integer -> Integer -> Bool
addSNatProp x y =
  let
    x' = E8.intToSNat x
    y' = E8.intToSNat y
  in (x+y) == (E8.sNatToInt $ E8.addSNat x' y')

-- | QuickCheck proposition for testing Exercises08.multSNat
multSNatProp :: Integer -> Integer -> Bool
multSNatProp x y =
  let
    x' = E8.intToSNat x
    y' = E8.intToSNat y
  in (x*y) == (E8.sNatToInt $ E8.multSNat x' y')

-- | QuickCheck proposition for testing Exercises08.absSNat
absSNatProp :: Integer -> Bool
absSNatProp x =
  let
    x' = E8.intToSNat x
    n = E8.absSNat x'
    unwrapSign (E8.Positive n) = n
    unwrapSign (E8.Negative n) = n
  in case n of
       (E8.Positive n') -> unwrapSign x' == n'
       (E8.Negative _) -> False

-- | QuickCheck proposition for testing Exercises08.negateSNat
negateSNatProp :: Integer -> Bool
negateSNatProp x =
  let
    x' = E8.intToSNat x
    n  = E8.negateSNat x'
  in case x' of
       (E8.Positive x'') -> case n of
                              (E8.Negative n') -> x'' == n'
                              _ -> False
       (E8.Negative x'') -> case n of
                              (E8.Positive n') -> x'' == n'
                              _ -> False


-------------------------------------------------------------------------------------------
-- * Run Tests
main :: IO ()
main = hspec $ do
  describe "sNatToInt and intToSNat" $ do
    it "(sNatToInt (intToSNat x)) should be x" $ property $ natConvertProp
  describe "addSNat" $ do
    it "addSNat (intToSNat x) (intToSNat y) should be x+y" $ property $ addSNatProp
  describe "multSNat" $ do
    it "multSNat (intToSNat x) (intToSNat y) should be x*y" $ property $ multSNatProp
  describe "absSNat" $ do
    it "should always be Positive and not change the underlying Nat" $ property $ absSNatProp
  describe "negateSNat" $ do
    it "should flip the sign but keep the underlying Nat" $ property $ negateSNatProp
