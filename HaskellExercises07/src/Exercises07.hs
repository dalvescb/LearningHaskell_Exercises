{-|
Module      : HaskellExercises07.Exercises07
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Haskell exercise template Set 07 - McMaster CS 1JC3 2021
-}
module Exercises07 where

import Prelude hiding (curry,fmap)

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------
macid = "TODO"

-- Exercise A
-----------------------------------------------------------------------------------------------------------
-- Implement the function iter that iterates the application of a function, i.e. iter n f creates a new
-- function, that calls f on its arguments n times
-- E.x.
--   iter 3 f == f . f . f
-----------------------------------------------------------------------------------------------------------
iter :: Int -> (a -> a) -> (a -> a)
iter n f = error "TODO implement iter"

-- Exercise B
-----------------------------------------------------------------------------------------------------------
-- Implement the function concatMapMaybe that works like map but over the Maybe a type instead of [a]
-----------------------------------------------------------------------------------------------------------
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f m = error "TODO implement mapMaybe"

-- Exercise C
-----------------------------------------------------------------------------------------------------------
-- Implement the function concatMapMaybe that takes a function that returns a Maybe type, maps it over a
-- list, and keeps only the Just values (Hint: use concatMap and a function that converts Maybe a to [a])
-- E.x. concatMapMaybe (\x -> if x >= 0 then Just x else Nothing) [-1,0,2,-5]
--    == [0,2]
-----------------------------------------------------------------------------------------------------------
concatMapMaybe :: (a -> Maybe a) -> [a] -> [a]
concatMapMaybe f xs = error "TODO: implement concatMapMaybe"

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList (Nothing)  = []

-- Exercise D
-----------------------------------------------------------------------------------------------------------
-- Implement the function curry that takes an uncurried function and converts it into a curried on
-- E.x. (curry fst) 0 1 == 0
-- NOTE use a lambda expression, and the implementation is VERY straight forward
-----------------------------------------------------------------------------------------------------------
curry :: ((a,b) -> c) -> a -> b -> c
curry f = error "TODO: implement curry"

-- Exercise E
-----------------------------------------------------------------------------------------------------------
-- Implement the function foldt on the following Tree data type, that folds a tree from the left-most tree
-- to the right
-----------------------------------------------------------------------------------------------------------
data Tree a = TNode a [Tree a]
  deriving (Show,Eq)

foldt :: (b -> a -> b) -> b -> Tree a -> b
foldt op v (TNode x ts) =
  let
    v' = foldl (foldt op) v ts
  in error "TODO: implement foldt"
     -- NOTE if you wrap your head around whats going on here, the sol'n is
     -- super simple

-- Exercise F
-----------------------------------------------------------------------------------------------------------
-- Implement the function familyTree2Tree that converts the following FamilyTree data type to the
-- previously defined Tree data typed
-- NOTE you can avoid manually pattern matching on Maybe constructors by using a combination of mapMaybe
--      and converting Maybe a to [a]
-----------------------------------------------------------------------------------------------------------
data FamilyTree = Person { name   :: String
                         , mother :: Maybe FamilyTree
                         , father :: Maybe FamilyTree }
  deriving (Show,Eq)

familyTree2Tree :: FamilyTree -> Tree String
familyTree2Tree familyTree =
  let
    maybeMother = mother familyTree
    maybeFather = father familyTree
    t0 = error "TODO: implement familyTree2Tree"
    t1 = error "TODO: impleemnt familyTree2Tree"
  in TNode (name familyTree) (t0 ++ t1)

-- Exercise G
-----------------------------------------------------------------------------------------------------------
-- Implement a function all family that takes a FamilyTree and returns a list of every person in the
-- family using foldt
-- NOTE first convert to a Tree, then use foldt. The only trick is figuring out what to use for op
-----------------------------------------------------------------------------------------------------------
allFamily :: FamilyTree -> [String]
allFamily  familyTree =
  let
    tree = familyTree2Tree familyTree
    op   = error "TODO: implement allFamily"
    v    = error "TODO: implement allFamily"
  in foldt op v tree
