module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck 
import SetOrd
import Lecture4

-- ================================
-- === Set Int Random Generator ===
-- ================================
{-
randInt = generate arbitrary :: IO Int

-- genRandomIntSet :: Int -> Set
genRandomIntSet x = genRandomIntSet' x emptySet

genRandomIntSet' 0 s = do 
	return s
genRandomIntSet' x s = do
	nextInt <- randInt
	sNext <- insertSet nextInt s
	xNext <- if inSet nextInt s then x else x-1
	return xNext sNext

setQuickTest :: [Int] -> Bool
setQuickTest xs = 
	let xSet = list2set xs
	in (and $ zipWith inSet xs xSet) && ()
-}

-- ================================
-- === 5: SymClos Relations ==== 20 mins
-- ================================
type Rel a = [(a,a)]

-- the symmetric closure of a set can be reached by including the reverse of each relation in the set, we do this by adding the reverse, followed by removing any duplicates and sorting these two items and the symmetric closure of the tail.
symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):xs) = sort $ nub ((x,y):(y,x):(symClos xs))


-- ===============================
-- === 6: trClos Relations ==== 20 mins
-- ===============================
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- helper function that finds all of the missing transitive relations in a set, one layer deep. It also sorts and removes duplicates to maintain set properties
trClosFunc :: Ord a => Rel a -> Rel a
trClosFunc a = nub $ sort (a ++ (a @@ a))

-- the transitive closure of a set can be found by finding missing layers until adding a missing layer does not change the set, because if adding the next layer equals adding the empty set, we have no more missing items to insert into the set. If no items are missing, this means we are done.
trClos :: Ord a => Rel a -> Rel a
trClos a = fp' trClosFunc a