{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : HaskellExercises07.Spec
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises07 and a main function that runs each tests and prints the results
-}
module Main where

import qualified Exercises07 as E7
import Test.QuickCheck (quickCheck
                       ,quickCheckResult
                       ,quickCheckWithResult
                       ,stdArgs
                       ,maxSuccess
                       ,Result(Success)
                       ,within
                       ,Testable, Arbitrary (arbitrary), Positive (..))
import Test.Hspec
import Test.QuickCheck.Property (property)
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
instance Arbitrary a => Arbitrary (E7.Tree a) where
  arbitrary =
    let
      genTree n
        | n <= 0 = do v <- arbitrary
                      return $ E7.TNode v []
        -- NOTE always generates a binary tree
        | otherwise = do (Positive n0) <- arbitrary
                         t0 <- genTree (n `div` (n0+1)) -- generate an arbitrary sized left tree
                         (Positive n1) <- arbitrary
                         t1 <- genTree (n `div` (n1+1)) -- generate an arbitrary sized right tree
                         v <- arbitrary
                         return $ E7.TNode v [t0, t1]
      in sized genTree

-- | Arbitrary instance for Tree
instance Arbitrary E7.FamilyTree  where
  arbitrary =
    let
      genTree n
        | n <= 0 = do n <- arbitrary
                      return $ E7.Person n Nothing Nothing
        -- NOTE always generates a binary tree
        | otherwise = do (Positive n0) <- arbitrary
                         t0 <- genTree (n `div` (n0+1)) -- generate an arbitrary sized left tree
                         (Positive n1) <- arbitrary
                         t1 <- genTree (n `div` (n1+1)) -- generate an arbitrary sized right tree
                         n <- arbitrary
                         return $ E7.Person n (Just t0 )  (Just t1)
      in sized genTree

-- | Boolean implication
(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y
infixr 4 ==>

-- | QuickCheck proposition for testing Exercises07.iter
iterProp0 :: Int -> Bool
iterProp0 n
 | n <= 0 = True
 | otherwise = (E7.iter n (+1) 0) == n

iterProp1 :: Int -> Bool
iterProp1 n
 | n <= 0 = True
 | otherwise = (E7.iter n (*2) 1) == 2^n

-- | QuickCheck proposition for testing Exercises07.mapMaybe
mapMaybeProp :: Maybe Int -> Bool
mapMaybeProp maybe = (E7.mapMaybe (+1) maybe) == fmap (+1) maybe

-- | QuickCheck proposition for testing Exercises07.concatMapMaybe
concatMapMaybeProp0 :: [Int] -> Bool
concatMapMaybeProp0 xs = E7.concatMapMaybe Just xs == xs

concatMapMaybeProp1 :: [Int] -> Bool
concatMapMaybeProp1 xs = E7.concatMapMaybe (\_ -> Nothing) xs == []

-- | QuickCheck proposition for testing Exercises07.curry
curryProp0 :: Int -> Int -> Bool
curryProp0 x y = E7.curry snd x y == y

curryProp1 :: Int -> Int -> Bool
curryProp1 x y = E7.curry fst x y == x

-- | QuickCheck proposition for testing Exercises07.foldt
foldtProp0 :: E7.Tree Int -> Bool
foldtProp0 tree =
  let
    sumT (E7.TNode x ts ) = sum (x : (map sumT ts))
  in sumT tree == E7.foldt (+) 0 tree

foldtProp1 :: E7.Tree Int -> Bool
foldtProp1 tree =
  let
    prodT (E7.TNode x ts ) = product (x : (map prodT ts))
  in prodT tree == E7.foldt (*) 1 tree

-- | QuickCheck proposition for testing Exercises07.familyTree2Tree
familyTree2TreeProp :: E7.FamilyTree -> Bool
familyTree2TreeProp famTree  =
  let
    cmpTrees (E7.Person name (Just m) (Just f)) (E7.TNode x ts) = (name == x)
                                                                  && ( or (map (cmpTrees m) ts) )
                                                                  && ( or (map (cmpTrees f) ts) )

    cmpTrees (E7.Person name Nothing (Just f)) (E7.TNode x [t]) = (name == x)
                                                                  && ( cmpTrees f t )
    cmpTrees (E7.Person name (Just m) Nothing) (E7.TNode x [t]) = (name == x)
                                                                  && ( cmpTrees m t )
    cmpTrees (E7.Person name Nothing Nothing) (E7.TNode x [])   = (name == x)
    cmpTrees _ _ = False
  in cmpTrees famTree (E7.familyTree2Tree famTree)

-- | QuickCheck proposition for testing Exercises07.allFamily
allFamilyProp :: E7.FamilyTree -> Bool
allFamilyProp famTree =
  let
    famToList :: E7.FamilyTree -> [String]
    famToList (E7.Person n m f) = n : ((concat $ E7.maybeToList $ fmap famToList m)
                                      ++ (concat $ E7.maybeToList $ fmap famToList f))
  in and [ p `elem` famToList famTree | p <- E7.allFamily famTree ]
   && and [ p `elem` E7.allFamily famTree | p <- famToList famTree ]


-------------------------------------------------------------------------------------------
-- * Run Tests
main :: IO ()
main = hspec $ do
  describe "iter" $ do
    it "iter n (+1) 0 should be n" $ property $ iterProp0
    it "iter n (*2) 1 should be n" $ property $ iterProp1
  describe "mapMaybe" $ do
    it "mapMaybe (+1) should correspond to fmap (+1)" $ property $ mapMaybeProp
  describe "concatMapMaybe" $ do
    it "concatMapMaybe Just xs should be xs" $ property $ concatMapMaybeProp0
    it "concatMapMaybe (\\x -> Nothing) xs should be []" $ property $ concatMapMaybeProp1
  describe "curry" $ do
    it "curry snd x y should be y" $ property $ curryProp0
    it "curry fst x y should be x" $ property $ curryProp1
  describe "foldt" $ do
    it "foldt (+) 0 tree should sum all elements in tree" $ property $ foldtProp0
    it "foldt (*) 1 tree should multiply all elements in tree" $ property $ foldtProp1
  describe "familyTree2Tree" $ do
    it "every node in each tree should have the same parents" $ property $ familyTree2TreeProp
  describe "allFamily" $ do
    it "every name in the tree should appear in the list and vice versa" $ property $ allFamilyProp
