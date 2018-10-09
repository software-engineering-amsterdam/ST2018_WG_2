import Data.List
import Numeric
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


forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Task 1 

randomNumsList:: Int ->  [Float]
randomNumsList 0 = []
randomNumsList x = 
  let count = x
  in randomNumsList(x-1) ++ unsafePerformIO(probs 1)


countInInterval:: Float -> Float -> [Float] -> Int
countInInterval startInterval endInterval xs = length ([x | x <- xs, x > startInterval && x < endInterval])

-- Showing the percantage of numbers of random list of 10000 elements in each quatuple
frequencyDistribution :: (Int, Int, Int, Int)
frequencyDistribution = 
  let 
    list = randomNumsList 10000
    q1 = inPercentage (countInInterval 0 0.25 list) 
    q2 = inPercentage (countInInterval 0.25 0.5 list)
    q3 = inPercentage (countInInterval 0.5 0.75 list)
    q4 = inPercentage (countInInterval 0.75 1 list)
  in (q1,q2,q3,q4)

inPercentage:: Int -> Int
inPercentage a = round((fromIntegral a) / (fromIntegral 100))

-- Task 2 

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

isEquilateral:: (Int,Int,Int) -> Bool
isEquilateral (a,b,c) = 
    let firstCheck = (a == b)
        secondCheck = (a == c)
    in firstCheck && secondCheck

isRectangular:: (Int,Int,Int) -> Bool
isRectangular (a,b,c)  = a^2 + b^2 == c^2

isTriangle:: (Int,Int,Int) -> Bool
isTriangle (a,b,c) = 
    let firstCheck = (a + b) > c
        secondCheck = (a + c) > b
        thirdCheck = b + c > a
  in firstCheck && secondCheck && thirdCheck

isIscosceles:: (Int,Int,Int) -> Bool
isIscosceles (a,b,c) = 
  let firstCheck = (a == b)
      secondCheck = (a == c) 
      thirdCheck = (b == c)
  in isEquilateral (a,b,c) == False && (firstCheck || secondCheck || thirdCheck)


triangle:: Int -> Int -> Int -> Shape
triangle a b c = 
    if (isTriangle(a,b,c) == False) then NoTriangle
    else if (isEquilateral(a,b,c)) then Equilateral
    else if (isRectangular(a,b,c)) then Rectangular
    else if (isIscosceles(a,b,c)) then Isosceles
    else Other

pythTriples :: [(Int,Int,Int)]
pythTriples = filter (\ (x,y,z) -> x^2 + y^2 == z^2)
                    [ (x,y,z) | z <- [1..], x <- [1..z], y <- [1..z], x < y]

test2a = quickCheck (\ (Positive a) (Positive b) (Positive c) -> triangle a b (a+b+c) == NoTriangle)
test2b = quickCheck (\ (Positive a) -> triangle a a a == Equilateral)

-- Source: Team 1 
-- think 1 more time
test2d = quickCheck (\ (Positive a) -> triangle (2*a) (2*a) ((4*a)-1)  == Isosceles) -- IDK WHY
test2c = all (\(x,y,z) -> triangle x y z == Rectangular) (take 100 pythTriples)

-- Task 3

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

domain = [-10..10]

test1 = stronger domain (\ x -> even x && x > 3) even
test2 = stronger domain (\ x -> even x || x > 3) even
test3 = stronger domain (\ x -> (even x && x > 3) || even x) even
test4 = stronger domain even (\ x -> (even x && x > 3) || even x) 

-- prop1 :: Int -> Bool
-- prop1 x = x > 3 && even x

-- prop2 :: Int -> Bool
-- prop2 x = x > 3 || even x

-- prop3 :: Int -> Bool
-- prop3 x = (even x && x > 3) || even x

-- prop3 :: Int -> Bool
-- prop3 x = (even x && x > 3) || even x

-- TO DO: Finish the task

--Properties 

-- Task 4 

perms :: [a] ->[[a]]
perms [] = [[]] 
perms (x:xs) = 
    concat (map (insrt x) (perms xs)) where 
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

-- Only working for Ints 

isPermutation:: [Int] -> [Int] ->  Bool  -- first parametr gets the list we are checking if it is permutation / secon parameter is the list that a permutation should be constructed
isPermutation x y = elem x (permutations y)

-- Works with strings
isPermutation2 :: Eq a => [a] -> [a] -> Bool
isPermutation2 xs ys = length xs == length ys && forall xs (\x -> elem x ys) && forall ys (\y -> elem y xs)

-- Properties
-- Equal size of the compared elements
-- Equal elements/characters in the input (capital letters matters)

--Manual Tests
manualTests = do 
    putStrLn "The length of the two elements should be equal"
    putStrLn "Different Length of 2 elements ('Elitsa' & 'Eli'), so isPermutation2 should return false"
    putStrLn (show (isPermutation2 "Elitsa" "Eli"))
    putStrLn "Equal Length of 2 elements ('Elitsa' & 'Elitsa'), so isPermutation2 should return true"
    putStrLn (show (isPermutation2 "Elitsa" "Elitsa"))
    putStrLn "Elements in both should be the same (capital letters as well) so test for elements 'Elitsa' and 'letsai' should return false"
    putStrLn (show (isPermutation2 "Elitsa" "letsai"))
-- More test in this sense could be done

-- QuickCheck Test
isPermutationTest:: Int -> Int -> Int -> Bool
isPermutationTest x y z = 
                  let list = [x,y,z]
                  in isPermutation2 list (head (permutations list))

--Source Team 7 
isPermutationTest2 :: Int -> Int -> Int -> Bool
isPermutationTest2 a b c = forall (permutations list) (isPermutation2 list) where
                           list = [a, b, c]

-- Task 5
isDerangement:: [Int] -> [Int] -> Bool
isDerangement [] [] = True
isDerangement (x:xs) (y:ys) = if x == y then False else isDerangement xs ys

deran :: Int -> [[Int]]
deran n = 
    let (x:xs) = permutations [0..n-1] 
    in filter (isDerangement x) xs



-- Testable properties 
-- The lenght of both list are the same
-- The elements in the both list are the same
-- Every element is on another place

testDerangement :: Bool
testDerangement = and [
                not (isDerangement [1,2,3] [3,2,1]),
                isDerangement [1,2,3] [2,3,1],
                not (isDerangement [1,2,3] [1,2,4]),
                not (isDerangement [1,2] [1,2,3])
                ]

-- Needed further explanation
isDerangementTest :: Int -> Bool
isDerangementTest n = apply (n <= 1 && n > 10) (-->) (and (map (isDerangement [0..n-1]) (deran n)))

apply :: a -> (a -> b -> c) -> b -> c
apply a f b = f a b

-- Task 6 - Implementing and testing ROT13 encoding

alphabet = ['a'.. 'z']

getIndex:: (Eq a) => a -> [a] -> Int
getIndex l list = getIndex' l list 0 

getIndex'::(Eq a) => a -> [a] -> Int -> Int
getIndex' elem (x:xs) startingPoint = 
                      if elem == x then startingPoint
                      else getIndex' elem xs (startingPoint + 1) 

replaceChar:: Int -> Char 
replaceChar x = 
       let characterIfMoreThan13 = 13 - (26 - x)
       in  if (characterIfMoreThan13 >= 0 && x >= characterIfMoreThan13)  then alphabet !! characterIfMoreThan13
                 else alphabet !! (x + 13)

rot13:: [Char] -> [Char]
rot13 [] = []
rot13 list = replaceChar(getIndex(head list) alphabet) : rot13(tail(list)) 
      
      
-- Euler 
-- TASK 1
divisibleBy3or5:: Int -> Int
divisibleBy3or5  boundary = sum [x | x <- [1.. boundary], (mod x 5 == 0 || mod x 3 == 0) && (x < boundary)]

-- Task 2

fibonacci:: Int
fibonacci = sum (filter (even) (fibonacci' 0 1))

fibonacci':: Int -> Int -> [Int]
fibonacci' x y = 
              let numberToAppenad = x + y
              in if numberToAppenad < 4000000 -- restriction provided in the task 
                 then numberToAppenad : fibonacci' y numberToAppenad  
              else []




      