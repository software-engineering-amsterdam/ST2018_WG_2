module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck 
import SetOrd
import Lecture4

-- =================================
-- === 1: Questions Chapter 4 ==== 1:30 hours (average)
-- =================================
-- Group: Resolved most questions between each other with explanation
-- JM: I have found no points that cause difficulty of understanding
-- MS: No questions
-- KL: -- p. 120 The Russell Paradox proof
-- p. 121 ex. 4.6 and 4.7
-- p. 125 ex. 4.8 + Remark
-- p.126 ex. 4.11
-- p. 134 notation last line of exercise 4.25 + ex. 4.27
-- p. 135 ex. 29-35
-- p. 137 defining ordered pairs, Notation? Why is that so?
-- p. 139 Prelude type
-- p. 142 first Haskell part, why do they do it? Just a definition?
-- p. 144 ex. 47
-- p. 158 haskell part
-- EV: 

-- ======================================
-- === 2: Random IntSet Generator ==== 5 hours
-- === Most of the time spent on reading about monads and finding out how QC generators work
-- ======================================


-- ============
-- Generator 1
-- ============
-- generates a random set of size <= n, where numbers are in the range [1,1000]
generateRandomSet :: Int -> IO (Set Int)
generateRandomSet 0 = return emptySet
generateRandomSet n = do
             randomInt <- randomRIO (1, 1000)
             restOfSet <- generateRandomSet (n-1) 
             return (insertSet randomInt restOfSet)


-- ============
-- Generator 2
-- ============

--adds support for QuickCheck arbitrary set generation
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary  = 
                do
                    list <- arbitrary
                    return $ list2set list

-- generator 2 tester
exercise2 = do 
    print "Set generator based on Lab 2, produces single set with <= 20 elements: "
    set <- generateRandomSet 20
    print set
    print "Set generator using Arbitrary, compatible with QuickCheck, used in later tests."
    print "Sample of generated sets: "
    sample $ (arbitrary :: Gen (Set Int))


--TEST REPORT
-- *Lab4> exercise2
-- "Set generator based on Lab 2, produces single set with <= 20 elements: "
-- {112,185,200,209,220,247,260,274,371,413,511,639,649,672,689,710,714,729,869,892}
-- "Set generator using Arbitrary, compatible with QuickCheck, used in later tests."
-- "Sample of generated sets: "
-- {}
-- {-2}
-- {}
-- {-5,-1,1}
-- {2}
-- {-1,6,8}
-- {-10,-8,-7,-4,-1,4,5,6,8,9}
-- {-14,-11,-7,5,11}
-- {13}
-- {-15,-14,-12,-9,-3,2,10}
-- {-20,-17,-16,-14,-10,-6,-4,-1,3,4,12,19}


-- =========================================
-- === 3: intersect, union, and diff ==== 2:00 hours
-- =========================================
-- The intersection of a set can be computed as all of the elements of set1 that also occur in set2
setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection (Set set1) (Set set2) = (Set [x | x <- set1, x `elem` set2])

-- The union can be computed by just inserting every element from set1 into set2. The definition of the given Set datatype will take care of removing any duplicates.
setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set []) set2 = set2
setUnion (Set (s:ss)) set2 = setUnion (Set ss) (insertSet s set2)

-- the difference of a set is the set containing all elements that occur in either one of the given sets, but not both.
setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference (Set set1) (Set set2) = 
    (Set [x | x <- set1, not(x `elem` set2)])

set2List :: Set z -> [z]
set2List (Set a) = a

propertyIntersectionElem :: Eq a => Set a -> Set a -> Set a -> Bool
propertyIntersectionElem (Set a) (Set b) (Set res) = forall res (\x -> elem x a) && forall b (\x -> elem x b)

propertyIntersectionLength :: Eq a => Set a -> Set a -> Set a -> Bool
propertyIntersectionLength (Set a) (Set b) (Set res) =  (length res <= length a) && (length res <= length b)

propertyUnionElem :: Eq a => Set a -> Set a -> Set a -> Bool
propertyUnionElem (Set a) (Set b) (Set res) =  forall res (\x -> elem x a || elem x b)

propertyUnionLength :: Eq a => Set a -> Set a -> Set a -> Bool
propertyUnionLength (Set a) (Set b) (Set res) = ((max (length a) (length b)) <= (length res))  && (length res <= (length a + length b))


propertyDifferenceElem :: Eq a => Set a -> Set a -> Set a -> Bool
propertyDifferenceElem (Set a) (Set b) (Set res) =  forall res (\x -> (elem x a) && (not $ elem x b)) 

propertyDifferenceLength :: Eq a => Set a -> Set a -> Set a -> Bool
propertyDifferenceLength (Set a) (Set b) (Set res) = (length res) <= (length a)

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- no duplicates in results, verifies that the underlying
-- data structure invariant was preserved
setInvariantNoDuplicatesProperty :: Eq a => Set a -> Bool
setInvariantNoDuplicatesProperty (Set []) = True
setInvariantNoDuplicatesProperty (Set (x:xs)) = if elem x xs then False else setInvariantNoDuplicatesProperty (Set xs)

exercise3 = do 
    print "Testing properties of set intersection using own generator..."
    setA <- generateRandomSet 10
    setB <- generateRandomSet 5
    print $ propertyIntersectionElem setA setB (setIntersection setA setB)
    print $ propertyIntersectionLength setA setB (setIntersection setA setB)
    print "Testing properties of set union using own generator..."
    print $ propertyUnionElem setA setB (setUnion setA setB)
    print $ propertyUnionLength setA setB (setUnion setA setB)
    print "Testing properties of set difference using own generator..."
    print $ propertyDifferenceElem setA setB (setDifference setA setB)
    print $ propertyDifferenceLength setA setB (setDifference setA setB)

    print "Testing properties of set intersection using QuickCheck..."
    quickCheckResult(\setA setB -> propertyIntersectionElem setA setB (setIntersection (setA::(Set Int)) (setB::(Set Int))))
    quickCheckResult(\setA setB -> propertyIntersectionLength setA setB (setIntersection (setA::(Set Int)) (setB::(Set Int))))
    quickCheckResult(\setA setB -> setInvariantNoDuplicatesProperty (setIntersection (setA::(Set Int)) (setB::(Set Int))))


    print "Testing properties of set union using QuickCheck..."
    quickCheckResult(\setA setB -> propertyUnionElem setA setB (setUnion (setA::(Set Int)) (setB::(Set Int))))
    quickCheckResult(\setA setB -> propertyUnionLength setA setB (setUnion (setA::(Set Int)) (setB::(Set Int))))
    quickCheckResult(\setA setB -> setInvariantNoDuplicatesProperty (setUnion (setA::(Set Int)) (setB::(Set Int))))

    print "Testing properties of set difference using QuickCheck..."
    quickCheckResult(\setA setB -> propertyDifferenceElem setA setB (setDifference (setA::(Set Int)) (setB::(Set Int))))
    quickCheckResult(\setA setB -> propertyDifferenceLength setA setB (setDifference (setA::(Set Int)) (setB::(Set Int))))
    quickCheckResult(\setA setB -> setInvariantNoDuplicatesProperty (setDifference (setA::(Set Int)) (setB::(Set Int))))


--TEST REPORT
-- *Lab4> exercise3
-- "Testing properties of set intersection using own generator..."
-- True
-- True
-- "Testing properties of set union using own generator..."
-- True
-- True
-- "Testing properties of set difference using own generator..."
-- True
-- True
-- "Testing properties of set intersection using QuickCheck..."
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- "Testing properties of set union using QuickCheck..."
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- "Testing properties of set difference using QuickCheck..."
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}
-- *Lab4> 



-- =================================
-- === 4: Questions Chapter 5 ==== 1 hours (average)
-- =================================
-- Question: Difference between 'R_{<=}' and '<=', is it only notation?
-- Question: Equivalence classes: Example 5.76, what is the modulo equivalence? 
-- Question: How can we define 'equivalence modulo ~' ? 
-- how to solve: p. 166 ex 17
-- p. 167 ex 23
-- p. 171 ex 36

-- ================================
-- === 5: SymClos Relations ==== 20 mins
-- ================================
type Rel a = [(a,a)]

-- the symmetric closure of a set can be reached by including the reverse of each relation in the set, we do this by adding the reverse, followed by removing any duplicates and sorting these two items and the symmetric closure of the tail.
symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):xs) = sort $ nub ((x,y):(y,x):(symClos xs))


-- ===============================
-- === 6: trClos Relations ==== 1:00 hours
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

-- helper function to determine whether a Relation is symmetric.
isSymmetric :: (Eq a) => Rel a -> Bool
isSymmetric r = forall r (\(a,b) -> elem (b,a) r)

--tests if rleationship is a set (duplicate free)
isDuplicateFree :: Eq a => Rel a -> Bool
isDuplicateFree list = (length $ nub list) == (length list)

-- helper function to determine whether a closure is actually a closure, we test this by seeing if the removal of any element from the relation gives a relation for which the funciton still holds. If this is the case, we have an extra element that is unnecessary, so our closure is not perfect. We exempt relation with only one or zero elements, since these are closures per definition 
isClosure :: (Eq a) => (Rel a -> Bool) -> Rel a -> Rel a -> Bool
isClosure function original r = 
    original == r || (length r < 2) || (not $ and $ map function [delete x r | x <- r])

{- trClosTest takes a list of integer tuples that will be transformed into a set. 
This list will be generated by quickCheck. When we have our testSet, we generate the transitive closure by using our trClos function. 
We test its validity by checking whether the original set is a subset of the result, the result is transitive, and we see if we remove any element from the newly added items 
(the difference of the original and the closure) the result is still transitive. 
If this last one is the case, we haven't found the best closure possible and our implementation is flawed.
-}

trClosTest :: (Eq a, Ord a) => Rel a -> Bool
trClosTest relation =
    let transitiveClosure = trClos relation
    in  subSet (Set relation) (Set transitiveClosure) && 
        isTransitive transitiveClosure &&
        isClosure isTransitive relation transitiveClosure &&
        isDuplicateFree transitiveClosure


{- symClosTest takes a list of integer tuples that will be transformed into a set. 
This list will be generated by quickCheck. When we have our testSet, we generate the symmetric closure by using our symClos function. 
We test its validity by checking whether the original set is a subset of the result, the result is symmetric, and we see if we remove any element from the newly added items (the difference of the original and the closure) the result is still symmetric. 
If this last one is the case, we haven't found the best closure possible and our implementation is flawed.
-}
symClosTest :: (Eq a, Ord a) => Rel a -> Bool
symClosTest relation =
    let symmetricClosure = symClos relation
    in  subSet (Set relation) (Set symmetricClosure) && 
        isSymmetric symmetricClosure && 
        isClosure isSymmetric relation symmetricClosure && 
        isDuplicateFree symmetricClosure



exercise7 = do
    print "Testing properties of symetric closure"
    quickCheckResult(\originalRel -> symClosTest (symClos (originalRel::(Rel Int))))

    print "Testing properties of transitive closure"
    quickCheckResult(\originalRel -> trClosTest (symClos (originalRel::(Rel Int))))

{-
*Lab4> exercise7
"Testing properties of symetric closure"
+++ OK, passed 100 tests.
"Testing properties of transitive closure"
+++ OK, passed 100 tests.
Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}
*Lab4> 

-}

-- ======================================
-- === 8: sym(tr(R)) =?= tr (sym(R)) ====
-- ======================================

-- we can check this fairly easily by defining a function that makes use of quickCheck to find counterexamples. If the sym of the tr of any Rel is not the same as its tr of the sym, then we have found a counter example.
symTrIsTrSymCheck :: [(Int, Int)] -> Bool
symTrIsTrSymCheck a = (symClos $ trClos a) == (trClos $ symClos a)

exercise8 = do 
    print "Counteraxample of exercise 8: "
    quickCheck symTrIsTrSymCheck

{-
*Lab4> quickCheck symTrIsTrSymCheck
*** Failed! Falsifiable (after 3 tests and 4 shrinks):    
[(0,1)]

quickCheck has found is a counterExample, the set [(0,1)]. This is a valid counterExample since the transitive closure of [(0,1)] is again [(0,1)] and its symmetric is [(0,1),(1,0)]. But, the transitive closure of [(0,1),(1,0)] is [(0,0),(0,1),(1,0),(1,1)]. This means that
sym $ tr [(0,1)] = sym [(0,1),(1,0)] = [(0,0),(0,1),(1,0),(1,1)]
tr $ sym [(0,1)] = tr [(0,1)]        = [(0,1),(1,0)]
These two are not the same, hence a valid counterexample, hence there is a difference between the transitive of the symmetric and the symmetric of the transitive.
-}


