module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck 
import SetOrd
import Lecture4
import System.IO.Unsafe

-- =================================
-- === 1: Qustions Chapter 4 ==== 1 hours
-- =================================
-- I have found no points that cause difficulty of understanding

-- ======================================
-- === 2: Random IntSet Generator ==== 1 hours
-- ======================================

-- helper function to get random function
randInt' :: Int -> Int -> IO Int
randInt' low high = do
    value <- getStdRandom (randomR (low, high))
    return value

randInt :: Int -> Int -> Int
randInt a b = unsafePerformIO (randInt' a b)

-- function that generates a random set of at most 15 elements
randomSet :: Set Int
randomSet = randomSet' 15 emptySet

randomSet' :: Int -> Set Int -> Set Int
randomSet' 0 s = s
randomSet' x s = randomSet' (x-1) (insertSet (randInt 0 100) s)

-- helper function that checks if a list is sorted
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted (x:[]) = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- set tester function, if the generated set has at most as many items as the provided variable indicating the length of the set, the set does not contain any duplicates (removing all duplicates in the list does not change the length of the list), and the set is sorted, then we have a valid generator.
randomSetTest :: Int -> Bool
randomSetTest x =
    let len = (abs x) `mod` 30
        (Set s) = randomSet' len emptySet
    in  ((length s) <= len) &&
        (length s == (length $ nub s)) &&
        (isSorted s)
{-
Test Result:
*Lab4> quickCheck randomSetTest 
+++ OK, passed 100 tests.
-}

-- =========================================
-- === 3: intersect, union, and diff ==== 20 mins
-- =========================================
-- The intersection of a set can be computed as all of the elements of set1 that also occur in set2
myIntersect :: (Ord a) => Set a -> Set a -> Set a
myIntersect (Set set1) (Set set2) = (Set [x | x <- set1, x `elem` set2])

-- The union can be computed by just inserting every element from set1 into set2. The definition of the given Set datatype will take care of removing any duplicates.
myUnion :: (Ord a) => Set a -> Set a -> Set a
myUnion (Set []) set2 = set2
myUnion (Set (s:ss)) set2 = myUnion (Set ss) (insertSet s set2)

-- the difference of a set is the set containing all elements that occur in either one of the given sets, but not both.
myDiff :: (Ord a) => Set a -> Set a -> Set a
myDiff (Set set1) (Set set2) = 
    (Set [x | x <- (set1 ++ set2), not((x `elem` set2) && (x `elem` set1))])

-- @TODO tests


-- =================================
-- === 4: Qustions Chapter 5 ==== 1 hours
-- =================================
-- I have found no points that cause difficulty of understanding

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
-- === 7: trClos and symClos Test ==== 30 mins
-- ======================================
-- helper function to determine whether a Relation is transitive. If we construct a list of all of the transitive relationships that could occur in the original relation but don't, and the length of that list is 0, then we have a transitive relationship.
isTransitive :: (Eq a) => Rel a -> Bool
isTransitive r = (length [(a,d) | (a,b) <- r, (c,d) <- r, b == c, not((a,d) `elem` r)]) == 0

-- helper function to determine whether a Realtion is symmetric.
isSymmetric :: (Eq a) => Rel a -> Bool
isSymmetric r = (length [(b,a) | (a,b) <- r, not((b,a) `elem` r)]) == 0

-- helper function to determine whether a closure is actually a closure, we test this by seeing if the removal of any element from the relation gives a relation for which the funciton still holds. If this is the case, we have an extra element that is unnecessary, so our closure is not perfect. We exempt relation with only one or zero elements, since these are closures per definition 
isClosure :: (Eq a) => (Rel a -> Bool) -> Rel a -> Rel a -> Bool
isClosure function original r = 
    original == r || (length r < 2) || (not $ and $ map function [delete x r | x <- r])

-- trClosTest takes a list of integer tuples that will be transformed into a set. Since this list will be generated by quickCheck, we are going to have to make sure the lists aren't overly long, a max of 20 items should do it. When we have our testSet, we generate the transitive closure by using our trClos function. We test its validity by checking whether the original set is a subset of the result, the result is transitive, and we see if we remove any element from the newly added items (the difference of the original and the closure) the result is still transitive. If this last one is the case, we haven't found the best closure possible and our implementation is flawed.
trClosTest :: (Eq a, Ord a) => Rel a -> Bool
trClosTest relation =
    let transitiveClosure = trClos relation
    in  subSet (Set relation) (Set transitiveClosure) && 
        isTransitive transitiveClosure &&
        isClosure isTransitive relation transitiveClosure

-- wrapper that allows quickCheck to interact with the trClosTest, we take the first 20 elements from the quickCheck provided example to make sure the functions can be computed in an acceptable timeframe
trClosQuickTest :: [(Int, Int)] -> Bool
trClosQuickTest value = trClosTest $ sort $ take 20 value

{-
Test results:
*Lab4> quickCheck trClosQuickTest 
+++ OK, passed 100 tests.
-}

-- symClosTest takes a list of integer tuples that will be transformed into a set. Since this list will be generated by quickCheck, we are going to have to make sure the lists aren't overly long, a max of 20 items should do it. When we have our testSet, we generate the symmetric closure by using our symClos function. We test its validity by checking whether the original set is a subset of the result, the result is symmetric, and we see if we remove any element from the newly added items (the difference of the original and the closure) the result is still symmetric. If this last one is the case, we haven't found the best closure possible and our implementation is flawed.
symClosTest :: (Eq a, Ord a) => Rel a -> Bool
symClosTest relation =
    let symmetricClosure = symClos relation
    in  subSet (Set relation) (Set symmetricClosure) && 
        isSymmetric symmetricClosure && 
        isClosure isSymmetric relation symmetricClosure

-- wrapper that allows quickCheck to interact with the symClosTest, we take the first 20 elements from the quickCheck provided example to make sure the functions can be computed in an acceptable timeframe
symClosQuickTest :: [(Int, Int)] -> Bool
symClosQuickTest value = symClosTest $ sort $ take 20 value

{-
Test results:
*Lab4> quickCheck symClosQuickTest 
+++ OK, passed 100 tests.
-}

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


