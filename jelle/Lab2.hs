
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


-- =======
-- == 2 ==
-- =======
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
