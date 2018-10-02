module Lab4 where
 
import Data.List
import System.Random
import Test.QuickCheck  
import SetOrd
import System.Random
import Data.List (sort) 



-- ============================
-- == Ex 1: Chapter 4
-- == Time: 0:45 hours
-- ============================

{-
Questions: 
1. Is it possible to ever have 'a = {a}' in Haskell? 
-}



-- ============================
-- == Ex 2: Two generators
-- == Time: 4:00 hours
-- == Most of the time spent on reading about monads and finding out how generators work
-- ============================


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




{- Reading sources and code sources for below code:
http://geekyplatypus.com/y-u-have-no-code-samples-quickcheck/
https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators
-}

-- ============
-- Generator 2
-- ============
--adds support for QuickCheck arbitrary set generation
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary  = 
                do
                    list <- arbitrary
                    return $ Set (sort $ nub $ list)


exercise2 = do 
    print "Set generator based on Lab 2, produces single set with <= 20 elements: "
    randomSet <- generateRandomSet 20
    print randomSet
    print "Set generator using Arbitrary, compatible with QuickCheck, used in later tests."
    print "Sample of generated sets: "
    sample $ (arbitrary :: Gen (Set Int))




-- ============================
-- == Ex 3: Set intersection, union, difference
-- == Time: 2:00 hours
-- ============================

setIntersection, setUnion, setDifference :: (Ord a) => Set a -> Set a -> Set a


setIntersection (Set []) _ = emptySet
setIntersection _ (Set []) = emptySet
setIntersection (Set (a:ax)) setB = 
    if (inSet a setB) then insertSet a (setIntersection (Set ax) setB) 
        else setIntersection (Set ax) setB


setUnion setA (Set []) = setA
setUnion (Set []) setB = setB
setUnion (Set (a:ax)) setB = insertSet a (setUnion (Set ax) setB)


setDifference (Set []) _ = emptySet
setDifference setA (Set []) = setA
setDifference (Set (a:ax)) setB = if (inSet a setB) then setDifference (Set ax) setB
    else insertSet a (setDifference (Set ax) setB)


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



-- no duplicates in results, verifies that the underlying
-- data structure invariant was preserved
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


    print "Testing properties of set union using QuickCheck..."
    quickCheckResult(\setA setB -> propertyUnionElem setA setB (setUnion (setA::(Set Int)) (setB::(Set Int))))
    quickCheckResult(\setA setB -> propertyUnionLength setA setB (setUnion (setA::(Set Int)) (setB::(Set Int))))

    print "Testing properties of set difference using QuickCheck..."
    quickCheckResult(\setA setB -> propertyDifferenceElem setA setB (setDifference (setA::(Set Int)) (setB::(Set Int))))
    quickCheckResult(\setA setB -> propertyDifferenceLength setA setB (setDifference (setA::(Set Int)) (setB::(Set Int))))

-- ============================
-- == Ex 4: Chapter 5
-- == Time: 1:00 hours
-- ============================

{-
Questions: 
1. Difference between 'R_{<=}' and '<=', is it only notation?
-}



-- ============================
-- == Ex 5: Symmetric Closure
-- == Time: 0:30 hours
-- ============================

type Rel a = [(a,a)]

--TODO: use fold?, then use nub only oncex
--computes the symmetric closure of a relationship
symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((a,b):xs) = nub $ -- required to handle when input is [(x,y), ..., (y,x)]
    if (b,a) `elem` xs || a == b then (a,b):symClos xs --keep (a,b) and recurse
        else (a,b):(b,a):(symClos xs) --keep (a,b), add (b,a) and recurse


-- ============================
-- == Ex 6: Transitive Closure
-- == Time: 1:00 hours
-- ============================

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]


--computes the transitive closure of a relationship
trClos :: Ord a => Rel a -> Rel a 
trClos input = trClosureHelper $ sort $ nub input


--helper function to compute transitive closure
trClosureHelper :: Ord a => Rel a -> Rel a 
trClosureHelper input = fp (computeOneStepTransitivity) input

-- computes one step of relationship transitiveness by combining the original list
-- and combining with one immediate transitive steps
computeOneStepTransitivity :: Ord a => Rel a -> Rel a 
computeOneStepTransitivity list = sort $ nub $ ((list @@ list) ++ list)

--source: Lecture4.hs
fp :: Eq a => (a -> a) -> a -> a 
fp f = until (\ x -> x == f x) f


-- ============================
-- == Ex 7: Tests for exercise 5 and 6
-- == Time: 3:00 hours
-- ============================

--checks if left is subset of right list (right contains all elements of left)
containsAll :: Eq a => [a] -> [a] -> Bool
containsAll [] ys = True
containsAll (x:xs) ys = (elem x ys) && containsAll xs ys


forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

--tests if rleationship is a set (duplicate free)
isRelationshipDuplicateFree :: Eq a => Rel a -> Bool
isRelationshipDuplicateFree list = (length $ nub list) == (length list)

--tests if the relationship is trasitive (does not tst if it is transitive closure!)
isRelationshipTransitive :: Ord a => Rel a -> Bool
isRelationshipTransitive list = containsAll (list @@ list) list 

--tests if the relationship is symmetric
isRelationshipSymmetric :: Ord a => Rel a -> Bool
isRelationshipSymmetric list = forall list (\(a,b) -> elem (b,a) list)


--helper function to convert each (a,b) to (b,a)
invertRel :: (Ord a, Eq a) => Rel a -> Rel a
invertRel [] = []
invertRel ((a,b):xs) = (b,a):invertRel xs

--merges two relationships without duplicates
mergeRel :: (Ord a, Eq a) => Rel a -> Rel a -> Rel a
mergeRel [] b = b
mergeRel a [] = a
mergeRel a b = nub (a ++ b)


exercise7TestCases :: [(Rel Int, Rel Int)]
exercise7TestCases = [
    --(input, expected)
    --tests for symetric closure
    (symClos [(1,2)], [(1,2),(2,1)]),
    (symClos [(2,1),(1,2)], [(1,2),(2,1)]),

    --tests for transitive closure
    (trClos [(1,2),(2,3),(3,4)],[(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)])
    ]

exercise7TestCaseVerifier :: [Bool]
exercise7TestCaseVerifier = map (\(input, expected) -> (sort input) == (sort expected)) exercise7TestCases


exercise7Results = do 
    print "Testing symmetric closure being symetric using QuickCheck"
    quickCheckResult(\originalRel -> 
        let mergedInverse = mergeRel (originalRel::(Rel Int)) (invertRel originalRel) 
            in (sort $ nub mergedInverse::(Rel Int)) == (sort $ symClos originalRel) )
    quickCheckResult(\originalRel -> isRelationshipSymmetric (symClos (originalRel::(Rel Int))))
    print "Testing symmetric closure being superset of original relationship..."
    quickCheckResult(\originalRel -> let uniqueRel = nub originalRel in containsAll uniqueRel (symClos (uniqueRel::(Rel Int))))
    print "Testing symmetric closure being a set"
    quickCheckResult(\originalRel -> let uniqueRel = nub originalRel in isRelationshipDuplicateFree (symClos (uniqueRel::(Rel Int))))

    print "Testing transitive closure being transitive..."
    quickCheckResult(\originalRel -> isRelationshipTransitive $ trClos (originalRel::(Rel Int)) )
    print "Testing transitive closure being superset of original relationship..."
    quickCheckResult(\originalRel -> let uniqueRel = nub originalRel in containsAll uniqueRel (trClos (uniqueRel::(Rel Int))))
    print "Testing transitive closure being a set"
    quickCheckResult(\originalRel -> let uniqueRel = nub originalRel in isRelationshipDuplicateFree (trClos (uniqueRel::(Rel Int))))

    print "Testing manual test cases for symetric and transitive closure..."
    print (and exercise7TestCaseVerifier)

-- ============================
-- == Ex 8:
-- == Time: 0:30 hours
-- ============================

{-
Is there a difference between the symmetric closure of the transitive closure of a relation 

Answer: yes, we found a counteraxample:

 (sort $ nub $ trClos $ symClos [(1,2),(2,3),(3,4)] ) /=  (sort $ nub $ symClos $ trClos [(1,2),(2,3),(3,4)] )

-}
