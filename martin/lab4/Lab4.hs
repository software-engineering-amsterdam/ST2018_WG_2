module Lab4 where
 
import Data.List
import System.Random
import Test.QuickCheck  
--import SetOrd
import System.Random
import Data.List (sort) 



-- ============================
-- == Ex 1: Chapter 4
-- == Time: 0:00 hours
-- ============================

{-
Questions: 
-}


{-
arbitraryList :: Arbitrary a => Gen [a]
arbitraryList =
  sized $
    \n -> do
      k <- choose (0, n)
      sequence [ arbitrary | _ <- [1..k] ]
-}


{-
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary  = 
        sized $ \len -> do
                    actualLen <- choose (0, len)
                    list <- while (\(Set x) -> length x < actualLen) (insertSet arbitrary) (Set [])
                    return (Set list)
-}



--time: 1:30h

--source: Lab2.hs

--TODO: ensure the list length is == n?

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
--TODO: add sized $ ... 

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


--Test-Properties - Intersection
testIntersection setA setB = (intersectionPropertyLength setA setB) && (intersectionPropertyElements setA setB)
intersectionPropertyLength setA setB = ((length $ set2List $ setIntersection setA setB) <= (length $ set2List setA)) && ((length $ set2List $ setIntersection setA setB) <= (length $ set2List setB))
intersectionPropertyElements setA setB = subSet (setIntersection setA setB) setA && subSet (setIntersection setA setB) setB

--Test-Properties - Union
unionPropertyLength (Set a) (Set b) = ((length $ set2List $ setUnion (Set a) (Set b)) >= length a ) && (length $ set2List $ setUnion (Set a) (Set b)) >= length b && (length $ set2List $ setUnion (Set a) (Set b)) >= (length a + length b)
unionPropertyElements  (Set a) (Set b)=  subSet (Set a) (setUnion (Set a) (Set b)) && subSet (Set b) (setUnion (Set a) (Set b))

set2List :: Set z -> [z]
set2List (Set a) = a

--Test-Properties - Difference
differencePropertyLength (Set a) (Set b) = (length $ set2List $ setDifference (Set a) (Set b)) <= length a
differencePropertyElement (Set a) (Set b) = (not $ subSet (Set b) (setDifference (Set a) (Set b))) && subSet (setDifference (Set a) (Set b)) (Set a)


-- no duplicates in results, verifies that the underlying
-- data structure invariant was preserved
setInvariantNoDuplicatesProperty (Set []) = True
setInvariantNoDuplicatesProperty (Set (x:xs)) = if elem x xs then False else setInvariantNoDuplicatesProperty (Set xs)

exercise3 = do 
    print "Testing properties of set intersection using own generator..."
    print "Testing properties of set union using own generator..."
    print "Testing properties of set difference using own generator..."

    print "Testing properties of set intersection using QuickCheck..."
    print "Testing properties of set union using QuickCheck..."
    print "Testing properties of set difference using QuickCheck..."

-- ============================
-- == Ex 4: Chapter 5
-- == Time: 0:00 hours
-- ============================

{-
Questions: 
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


--source: Lecture4.hs
--while :: (a -> Bool) -> (a -> a) -> a -> a
--while = until . (not.)

--helper function to compute transitive closure
--precondition: x = sort $ nub x
--trClosureHelper :: Ord a => Rel a -> Rel a 
--trClosureHelper input = while (\x -> x /= computeOneStepTransitivity x) computeOneStepTransitivity input


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

{-
generateRandomTuple :: IO((Int,Int))
generateRandomTuple = do 
    x <- randomRIO (1, 1000)
    y <- randomRIO (1, 1000)
    return (x,y)

generateRandomRel :: Int -> IO (Rel Int)
generateRandomRel 1 = do 
    tuple <- generateRandomTuple
    return ([tuple])
generateRandomRel n = do
             randomTuple <- generateRandomTuple
             restOfList <- generateRandomRel (n-1) 
             return (randomTuple:restOfList)
-}

--helper function to convert each (a,b) to (b,a)
invertRel :: (Ord a, Eq a) => Rel a -> Rel a
invertRel [] = []
invertRel ((a,b):xs) = (b,a):invertRel xs

--merges two relationships without duplicates
mergeRel :: (Ord a, Eq a) => Rel a -> Rel a -> Rel a
mergeRel [] b = b
mergeRel a [] = a
mergeRel a b = nub (a ++ b)

--TODO: not same as containsAll?
--tests if left list is subset of right list
listSubSet :: Eq a => [a] -> [a] -> Bool
listSubSet [] [] = True
listSubSet (a:_) [] = False
listSubSet [] (b:_) = True
listSubSet (a:ax) b = if elem a b then listSubSet ax b else False

exercise7TestCases :: [(Rel Int, Rel Int)]
exercise7TestCases = [
    --(input, expected)
    --tests for symetric closure
    (symClos [(1,2)], [(1,2),(2,1)]),
    (symClos [(2,1),(1,2)], [(1,2),(2,1)]),
    --TODO

    --tests for transitive closure
    --TODO
    (trClos [(1,2),(2,3),(3,4)],[(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)])
    ]

exercise7TestCaseVerifier :: [Bool]
exercise7TestCaseVerifier = map (\(input, expected) -> (sort input) == (sort expected)) exercise7TestCases


exercise67Results = do 
    print "Testing symmetric closure being symetric using QuickCheck"
    quickCheckResult(\originalRel -> 
        let mergedInverse = mergeRel (originalRel::(Rel Int)) (invertRel originalRel) 
            in (sort $ nub mergedInverse::(Rel Int)) == (sort $ symClos originalRel) )
    quickCheckResult(\originalRel -> isRelationshipSymmetric (symClos (originalRel::(Rel Int))))
    print "Testing symmetric closure being superset of original relationship..."
    quickCheckResult(\originalRel -> let uniqueRel = nub originalRel in listSubSet uniqueRel (symClos (uniqueRel::(Rel Int))))
    print "Testing symmetric closure being a set"
    quickCheckResult(\originalRel -> let uniqueRel = nub originalRel in isRelationshipDuplicateFree (symClos (uniqueRel::(Rel Int))))

    print "Testing transitive closure being transitive..."
    quickCheckResult(\originalRel -> isRelationshipTransitive $ trClos (originalRel::(Rel Int)) )
    print "Testing transitive closure being superset of original relationship..."
    quickCheckResult(\originalRel -> let uniqueRel = nub originalRel in listSubSet uniqueRel (trClos (uniqueRel::(Rel Int))))
    print "Testing transitive closure being a set"
    quickCheckResult(\originalRel -> let uniqueRel = nub originalRel in isRelationshipDuplicateFree (trClos (uniqueRel::(Rel Int))))

    print "Testing manual test cases for symetric and transitive closure..."
    print (and exercise7TestCaseVerifier)





--        =================



{-- Sets implemented as ordered lists without duplicates --} 

newtype Set a = Set [a] deriving (Eq,Ord)

instance (Show a) => Show (Set a) where
    showsPrec _ (Set s) str = showSet s str

showSet []     str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
     where showl []     str = showChar '}' str
           showl (x:xs) str = showChar ',' (shows x (showl xs str))

emptySet  :: Set a       
emptySet = Set []

isEmpty  :: Set a -> Bool            
isEmpty (Set []) = True
isEmpty _        = False

inSet  :: (Ord a) => a -> Set a -> Bool  
inSet x (Set s) = elem x (takeWhile (<= x) s)

subSet :: (Ord a) => Set a -> Set a -> Bool
subSet (Set []) _       = True  
subSet (Set (x:xs)) set = (inSet x set) && subSet (Set xs) set 

insertSet :: (Ord a) => a -> Set a -> Set a 
insertSet x (Set s) = Set (insertList x s) 

insertList x [] = [x]
insertList x ys@(y:ys') = case compare x y of 
                                 GT -> y : insertList x ys' 
                                 EQ -> ys 
                                 _  -> x : ys 

deleteSet :: Ord a => a -> Set a -> Set a 
deleteSet x (Set s) = Set (deleteList x s)

deleteList x [] = []
deleteList x ys@(y:ys') = case compare x y of 
                                 GT -> y : deleteList x ys'
                                 EQ -> ys'
                                 _  -> ys

list2set :: Ord a => [a] -> Set a
list2set [] = Set []
list2set (x:xs) = insertSet x (list2set xs)
-- list2set xs = Set (foldr insertList [] xs)

powerSet :: Ord a => Set a -> Set (Set a)
powerSet (Set xs) = 
   Set (sort (map (\xs -> (list2set xs)) (powerList xs)))

powerList  :: [a] -> [[a]]
powerList  [] = [[]]
powerList  (x:xs) = (powerList xs) 
                     ++ (map (x:) (powerList xs))

takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (Set xs) = Set (take n xs) 

infixl 9 !!!

(!!!) :: Eq a => Set a -> Int -> a 
(Set xs) !!! n = xs !! n

unionSet :: (Ord a) => Set a -> Set a -> Set a 
unionSet (Set [])     set2  =  set2
unionSet (Set (x:xs)) set2  = 
   insertSet x (unionSet (Set xs) set2)
