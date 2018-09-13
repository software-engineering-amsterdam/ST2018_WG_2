
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import System.IO.Unsafe

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- getIndex :: (Eq a) => [a] -> a -> Int
-- getIndex l x = getIndex' l x 0

-- getIndex' [] _ _ = -1
-- getIndex' (a:as) x n = 
--     if a == x then n else getIndex' as x n+1

-- ===========================
-- == Recognizing Triangles == 1:30:00
-- ===========================
-- helper function to convert a list of length 3 to a tuple
listToTuple :: [Int] -> (Int,Int,Int)
listToTuple (a:b:c:[]) = (a,b,c)

-- helper function to get random integers in the domain [low, high]
randInt' :: Int -> Int -> IO Int
randInt' low high = do
    value <- getStdRandom (randomR (low, high))
    if True then return value else return 0

randInt :: Int -> Int -> Int
randInt a b = unsafePerformIO (randInt' a b)

triangle :: Int -> Int -> Int -> Shape
triangle a' b' c' = 
    -- we start by sorting the integers so that we know that a <= b <= c
    let (a,b,c) = listToTuple (sort [a',b',c'])
    in 
        -- if the two shorter sides don't reach the longer one, no triangle
        if a + b <= c then NoTriangle else
            -- if all sides are equally long, equilateral
            if (a == b) && (b == c) then Equilateral else
                -- if we have no equilateral, but two sides are equal, isoceles
                if (a==b) || (b==c) || (a==c) then Isosceles else
                    -- if we found a pythagorean triplet, rectangular. else: other.
                    if (a^2 + b^2 == c^2) then Rectangular else Other

-- perform five different tests with custom randint generators and compare
-- them all to find whether our triangle function is valid.
triangleTest = noTriangleTest && equilateralTest && isocelesTest
                && rectangularTest && otherTest

noTriangleTest :: Bool
noTriangleTest = 
    let testList = map (\_ -> (randInt 0 5, randInt 0 5, randInt 10 20)) [1..20]
    in length (filter (/= NoTriangle) (map (\(x,y,z) -> triangle x y z) testList)) == 0
equilateralTest :: Bool
equilateralTest = 
    let testList = map (\_ -> (let x = randInt 1 30 in (x,x,x))) [1..20]
    in length (filter (/= Equilateral) (map (\(x,y,z) -> triangle x y z) testList)) == 0
isocelesTest :: Bool
isocelesTest = 
    let testList = map (\_ -> (let x = randInt 20 30 in (x,x,randInt 10 15))) [1..20]
    in length (filter (/= Isosceles) (map (\(x,y,z) -> triangle x y z) testList)) == 0
rectangularTest :: Bool
rectangularTest = 
    let testList = map (\_ -> (let x = randInt 1 30 in (3*x,4*x,5*x))) [1..20]
    in length (filter (/= Rectangular) (map (\(x,y,z) -> triangle x y z) testList)) == 0
otherTest :: Bool
otherTest = 
    let testList = map (\_ -> (randInt 110 120, randInt 121 130, randInt 200 220)) [1..20]
    in length (filter (/= Other) (map (\(x,y,z) -> triangle x y z) testList)) == 0

-- =================================
-- == Testing Properties Strength == 1:00:00
-- =================================

newtype Prop1 = Prop1 (Int -> Bool)
instance Show Prop1 where
    show a = "prop1"
newtype Prop2 = Prop2 (Int -> Bool)
instance Show Prop2 where
    show a = "prop2"
newtype Prop3 = Prop3 (Int -> Bool)
instance Show Prop3 where
    show a = "prop3"
newtype Prop4 = Prop4 (Int -> Bool)
instance Show Prop4 where
    show a = "prop4"

prop1 :: Int -> Bool
prop1 x = x > 3 && even x
prop2 :: Int -> Bool
prop2 x = x > 3 || even x
prop3 :: Int -> Bool
prop3 x = (even x && x > 3) || even x
prop4 :: Int -> Bool
prop4 x = even x

stronger :: (Int -> Bool) -> (Int -> Bool) -> Bool
stronger p1 p2 =
    let r1 = map p1 [(-10)..10]
        r2 = map p2 [(-10)..10]
    in foldr (&&) True (zipWith (-->) r1 r2)

weaker :: (Int -> Bool) -> (Int -> Bool) -> Bool
weaker p1 p2 = stronger p2 p1

sortWith :: (a -> a -> Bool) -> [a] -> [a]
sortWith _ [] = []
sortWith f (x:xs) = sortWith f [i | i <- xs, f i x] ++ [x] 
                    ++ sortWith f [i | i <- xs, not (f i x)]

sortedProps :: [(Int -> Bool)]
sortedProps = sortWith stronger [prop1, prop2, prop3, prop4]

-- ==============================
-- == Recognizing Permutations == 1:00:00
-- ==============================
isPermutation :: [Int] -> [Int] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) y =
    if not $ elem x y then False else 
        isPermutation xs (filter (/= x) y)

-- =============================================
-- == Recognizing and Generating Derangements == 1:00:00
-- =============================================
-- only consider the two lists if their sorted versions are equivalent to each
-- other, this checks if the two lists contain the same elements. If this is
-- not the case, they cannot be derangements of each other.
isDerangement :: [Int] -> [Int] -> Bool
isDerangement x y = if (sort x) == (sort y) then isDerangement' x y else False

isDerangement' :: [Int] -> [Int] -> Bool
isDerangement' [] [] = True
isDerangement' (x:xs) (y:ys) = if x == y then False else isDerangement' xs ys

deran :: Int -> [[Int]]
deran n = 
    let (x:xs) = permutations [0..n-1] 
    in filter (isDerangement x) xs

derange :: [Int] -> [Int]
derange [] = []
derange (x:xs) = xs ++ [x]

isDerangementTest :: Int -> Bool
isDerangementTest n = n <= 1 || isDerangement [0..n-1] (derange [0..n-1])

deranPreCond :: [Int] -> Bool
deranPreCond l = length l > 1

deranPostCond :: [Int] -> [Int] -> Bool
deranPostCond x y = isDerangement x y