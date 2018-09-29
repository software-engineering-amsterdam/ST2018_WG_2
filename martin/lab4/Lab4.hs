module Lab4 where
 
import Data.List
import System.Random
import Test.QuickCheck  
import SetOrd
import System.Random


{-
Implement a random data generator for the datatype Set Int, where Set is as defined in SetOrd.hs. First do this from scratch, next give a version that uses QuickCheck to random test this datatype.
(Deliverables: two random test generators, indication of time spent.)
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



--sample $ (arbitrary :: Gen (Set Int))

--time: 1:30h

--source: Lab2.hs

--TODO: ensure the list length is == n?

generateProbSetList :: Int -> IO [Int]
generateProbSetList 0 = return []
generateProbSetList n = do
             randomInt <- randomRIO (1, 1000)
             restOfList <- generateProbSetList (n-1) 
             if elem randomInt restOfList 
                then return restOfList 
                else return (sort $ (randomInt:restOfList))
            --return (randomInt:restOfList)



generateRandomSet maxLength = do 
    set <- generateProbSetList maxLength
    return (Set set) 


--source: http://geekyplatypus.com/y-u-have-no-code-samples-quickcheck/
--source: https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
--source: https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators

--add support for QuickCheck arbitrary set generation
--TODO: add sized $ ... 
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary  = 
                do
                    list <- arbitrary
                    return $ Set (sort $ nub $ list)



{-
Implement operations for set intersection, set union and set difference, for the datatype Set defined in SetOrd.hs. Next, use automated testing to check that your implementation is correct. First use your own generator, next use QuickCheck.
-}

{-
(Set(..),emptySet,isEmpty,inSet,subSet,insertSet,
               deleteSet,powerSet,takeSet,(!!!),list2set,unionSet) 
-}


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





-- =================================

{-Suppose we implement binary relations as list of pairs, 
Haskell type [(a,a)]. Assume the following definition:
-}

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((a,b):xs) = 
    if (b,a) `elem` xs || a == b then (a,b):symClos xs --keep (a,b) and recurse
        else (a,b):(b,a):(symClos xs) --keep (a,b), add (b,a) and recurse

--symClos :: Ord a => (a,a) ->
--symClos' (a,b)

--that gives the symmetric closure of a relation, 
--where the relation is represented as an ordered list of pairs. 
--E.g., symClos [(1,2),(2,3),(3,4)] should give  [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].



{-
Use the datatype for relations from the previous exercise, plus

to define a function

 trClos :: Ord a => Rel a -> Rel a 
that gives the transitive closure of a relation, represented as an ordered list of pairs. E.g., trClos [(1,2),(2,3),(3,4)] should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
-}


infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]


--source: Lecture4.hs
while :: (a -> Bool) -> (a -> a) -> a -> a
while = until . (not.)


--computes the transitive closure of a relationship
trClos :: Ord a => Rel a -> Rel a 
trClos input = trClosure $ sort $ nub input

--helper function to compute transitive closure
--precondition: x = sort $ nub x
trClosure :: Ord a => Rel a -> Rel a 
trClosure input = while (\x -> x /= trClosOneStep x) trClosOneStep input
--TODO: use fixed point here

-- computes one step of relationship transitiveness by combining the original list
-- and combining with one immediate transitive steps
trClosOneStep :: Ord a => Rel a -> Rel a 
trClosOneStep list = sort $ nub $ ((list @@ list) ++ list)

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll [] ys = True
containsAll (x:xs) ys = (elem x ys) && containsAll xs ys


forall :: [a] -> (a -> Bool) -> Bool
forall = flip all


isRelationshipTransitive :: Ord a => Rel a -> Bool
isRelationshipTransitive list = containsAll (list @@ list) list 

isRelationshipSymetric :: Ord a => Rel a -> Bool
isRelationshipSymetric list = forall list (\(a,b) -> elem (b,a) list)

--1:45hrs 6+7 no tests


{-
Test the functions symClos and trClos from the previous exercises. Devise your own test method for this. Try to use random test generation. Define reasonable properties to test. Can you use QuickCheck? How?
-}

{-
exercise7ManualTestCases = [
    (trClos [], [])
    ]

exercise7ManualTestCaseVerifier = map (\(actual, expected) -> actual == expected) exercise7ManualTestCases
-}