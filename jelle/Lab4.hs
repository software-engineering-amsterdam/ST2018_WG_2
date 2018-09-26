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
-- === 6: trClos Relations ==== 30 mins
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

-- ======================================
-- === 8: sym(tr(R)) =?= tr (sym(R)) ====
-- ======================================

-- we can check this fairly easily by defining a function that makes use of quickCheck to find counterexamples. If the sym of the tr of any Rel is not the same as its tr of the sym, then we have found a counter example.
symTrIsTrSymCheck :: [(Int, Int)] -> Bool
symTrIsTrSymCheck a = (symClos $ trClos a) == (trClos $ symClos a)

{-
*Lab4> quickCheck symTrIsTrSymCheck
*** Failed! Falsifiable (after 3 tests and 4 shrinks):    
[(0,1)]

quickCheck has found is a counterExample, the set [(0,1)]. This is a valid counterExample since the transitive closure of [(0,1)] is again [(0,1)] and its symmetric is [(0,1),(1,0)]. But, the transitive closure of [(0,1),(1,0)] is [(0,0),(0,1),(1,0),(1,1)]. This means that
sym $ tr [(0,1)] = sym [(0,1),(1,0)] = [(0,0),(0,1),(1,0),(1,1)]
tr $ sym [(0,1)] = tr [(0,1)]        = [(0,1),(1,0)]
These two are not the same, hence a valid counterexample, hence there is a difference between the transitive of the symmetric and the symmetric of the transitive.
-}
