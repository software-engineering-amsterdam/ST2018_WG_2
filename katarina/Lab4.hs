module Lab4
where 

import Data.List
import Test.QuickCheck
import Data.Char
import Data.String
import SetOrd
import System.Random

-- Exercise 3 assumption: ordered

--module SetOrd (Set(..),emptySet,isEmpty,inSet,subSet,insertSet,
               --deleteSet,powerSet,takeSet,(!!!),list2set,unionSet) 

setIntersection :: (Ord a) => Set a -> Set a -> Set a 
setIntersection' :: (Ord a) => Set a -> Set a -> Set a 

setIntersection (Set []) _ = Set []
setIntersection _ (Set []) = Set []
setIntersection (Set (a:as)) (Set (bs)) = if (inSet a (Set bs)) then insertSet a (setIntersection (Set as) (Set bs)) else setIntersection (Set as) (Set bs)

-- only right if we are sure that the Set is ordered
setIntersection' (Set []) _ = Set []
setIntersection' _ (Set []) = Set []
setIntersection' (Set (a:as)) (Set (b:bs)) = if (a<b) then setIntersection' (Set as) (Set (b:bs)) else (if (b<a) then setIntersection' (Set (a:as)) (Set bs) else insertSet a (setIntersection' (Set as) (Set bs)))



setUnion::(Ord a) => Set a -> Set a -> Set a
setUnion (Set []) (Set a) = Set a
setUnion (Set a) (Set []) = Set a
setUnion (Set (a:as)) (Set (bs)) = if (inSet a (Set bs)) then setUnion (Set as) (Set bs) else insertSet a (setUnion (Set as) (Set bs))

setDifference::(Ord a) => Set a -> Set a -> Set a
setDifference (Set []) (Set a) = Set []
setDifference (Set a) (Set []) = Set a
setDifference (Set (a:as)) (Set (bs)) = if (inSet a (Set bs)) then setDifference (Set as) (Set bs) else insertSet a (setDifference (Set as) (Set bs))

--Test-Properties - Intersection
--testIntersection (Set a) (Set b) = intersectionPropertyLength (Set a) (Set b) && intersectionPropertyElements (Set a) (Set b)
--intersectionPropertyLength Set a Set b = length $ setIntersection Set a Set b <= length Set a && length $ setIntersection Set a Set b <= length Set b
intersectionPropertyElements (Set a) (Set b) = subSet (setIntersection (Set a) (Set b)) (Set a) && subSet (setIntersection (Set a) (Set b)) (Set b) 

--Test-Properties - Union
--unionPropertyLength Set a Set b = length $ setUnion Set a Set b >= length Set a && length $ setUnion Set a Set b >= length Set b && length $ setUnion Set a Set b >=  length Set a + length Set b
unionPropertyElements  (Set a) (Set b)=  subSet (Set a) (setUnion (Set a) (Set b)) && subSet (Set b) (setUnion (Set a) (Set b))

set2List :: Set z -> [z]
set2List (Set a) = a --a is of type [z]

--Test-Properties - Difference
differencePropertyLength (Set a) (Set b) = (length $ set2List $ setDifference (Set a) (Set b)) <= length a
differencePropertyElement (Set a) (Set b) = (not $ subSet (Set b) (setDifference (Set a) (Set b))) && subSet (setDifference (Set a) (Set b)) (Set a)


--no duplicates


--questions Chapter 4

-- p. 120 The Russell Paradox proof
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


-- questions Chapter 5
-- p. 166 ex 17
-- p. 167 ex 23
-- p. 171 ex 36
-- 