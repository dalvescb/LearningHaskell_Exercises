{-|
Module      : HaskellExercises06.Exercises06
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Haskell exercise template Set 06 - McMaster CS 1JC3 2021
-}
module Exercises06 where

import Prelude hiding (zipWith,foldr,foldl,concat,concatMap,lookup)

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
-- Implement the Higher Order Function zipWith that behaves like zip but instead of zipping a tuple,
-- uses a binary operator provided to it as an argument
-- E.x.  zipWith (*) [1,2,3] [0,0,0]
--      = [0,0,0]
-- NOTE QuickCheck test uses (+) as the function parameter
-----------------------------------------------------------------------------------------------------------
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith op xs ys = error "TODO implement zipWith"

-- Exercise B
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function foldr "folds" an operator to the right. For example
-- E.x.  foldr (/) 1 [18,27,3]
--      = 18 / (27 / (3 / 1))
-- NOTE QuickCheck test uses (-) as the function parameter
-----------------------------------------------------------------------------------------------------------
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op v xs = error "TODO implement foldr"

-- Exercise C
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function foldl "folds" an operator to the left. For example
-- E.x.  foldl (/) 18 [9,2,1]
--      = ((18 / 9) / 2) / 1
  -- NOTE QuickCheck test uses (-) as the function parameter
-----------------------------------------------------------------------------------------------------------
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl op v xs = error "TODO implement foldr"

-- Exercise D
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function concat (which takes a list of lists, and combines them into
-- just a list) using foldl
-----------------------------------------------------------------------------------------------------------
concat :: [[a]] -> [a]
concat xss = error "TODO implement concat"

-- Exercise E
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function concatMap using foldr. concatMap takes a function that returns a list,
-- maps that function of a list then concat's the result.
-- E.x.  concatMap (replicate 2) [1,2,3]
--     == [1,1,2,2,3,3]
  -- NOTE QuickCheck test uses (replicate 2) as the function parameter
-----------------------------------------------------------------------------------------------------------
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f xs  = error "TODO implement concatMap"

-- Exercise F
-----------------------------------------------------------------------------------------------------------
-- Implement the function lookup that takes a list of tuples, where the first element of the tuple
-- serves as a key and the second element a value (a list like this is also known as a dictionary), and
-- a key value, then looks up the first occuring element corresponding to that key. The return value is
-- wrapped in the Maybe type, so if the key doesn't occur anywhere in the list the function returns Nothing
-- E.x.  lookup 2 [(0,'a'),(1,'b')] == Nothing
--       lookup 2 [(0,'a'),(2,'b'),(2,'c')] == Just 'b'
-----------------------------------------------------------------------------------------------------------
lookup :: Eq k => k -> [(k,v)] -> Maybe v
lookup k0 ds = error "TODO implement lookup"
