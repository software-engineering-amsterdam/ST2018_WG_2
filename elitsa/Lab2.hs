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

prop1 :: Int -> Bool
prop1 x = x > 3 && even x

prop2 :: Int -> Bool
prop2 x = x > 3 || even x

prop3 :: Int -> Bool
prop3 x = (even x && x > 3) || even x

prop4 :: Int -> Bool
prop4 x = even x

container = [("prop1",prop1),("prop2",prop2),("prop3",prop3),("prop4",prop4)]

--Merge sort 
-- mergeSort:: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = 
              let listLenghtDivided = quot (length list) 2
                  firstHalf = take listLenghtDivided list
                  secondHalf = drop listLenghtDivided list
              in (orderTwoLists (mergeSort firstHalf) (mergeSort secondHalf))


orderTwoLists:: (Ord a) => [a] -> [a] -> [a]
orderTwoLists x [] = x
orderTwoLists [] y = y
orderTwoLists list1 list2 = if (head list1) < (head list2) 
                          then head list1 : orderTwoLists (tail list1) list2 
                          else head list2 : orderTwoLists list1 (tail list2) 

-- [3,27,38,43] [43]
-- [9,10,82] []

-- strongerCheck :: ([Char], (Int -> Bool)) -> ([Char], (Int -> Bool)) -> Bool
-- strongerCheck (_, p1) (_, p2) = stronger p1 p2

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

lowers = ['a'..'z']
capitals = ['A'.. 'Z']

getIndex:: (Eq a) => a -> [a] -> Int
getIndex l list = getIndex' l list 0 

getIndex'::(Eq a) => a -> [a] -> Int -> Int
getIndex' elem (x:xs) startingPoint = 
                      if elem == x then startingPoint
                      else getIndex' elem xs (startingPoint + 1) 

capitalLower:: Char -> Char
capitalLower char = if elem char lowers
                    then replaceChar (getIndex char lowers) lowers 
                    else if elem char capitals 
                    then replaceChar (getIndex char capitals) capitals
                    else char

replaceChar:: Int -> [Char] -> Char
replaceChar char alphabet = let charMore13 = 13 - (26 - char)
                 in  if (charMore13 >= 0 && char >= charMore13)  
                 then alphabet !! charMore13
                 else alphabet !! (char + 13)

rot13:: [Char] -> [Char]
rot13 xs = [ capitalLower x | x <- xs]

-- Specification 
-- The input size is equal to output size
-- Capital letters matter
-- Every element is replaced by the 13th letter after the input element
-- Rot13(Rot13) = original word

rot13test:: [Char] -> Bool
rot13test word = rot13 (rot13 word) == word 

--more test could be done but it is ok now.

-- TASK 8 - IBAN

-- Check that the total IBAN length is correct as per the country. If not, the IBAN is invalid
-- Move the four initial characters to the end of the string
-- Replace each letter in the string with two digits, thereby expanding the string, where A = 10, B = 11, ..., Z = 35
-- Interpret the string as a decimal integer and compute the remainder of that number on division by 97
--If the remainder is 1, the check digit test is passed and the IBAN might be valid.

moveBack:: [Char] -> [Char]
moveBack iban = drop 4 iban ++ take 4 iban

replaceLetter:: Char -> Int
replaceLetter ibanChar = (getIndex ibanChar capitals) + 10

replaceLettersInIban:: [Char] -> [Char]
replaceLettersInIban [] = []
replaceLettersInIban (x:xs) = if isNumber x
                              then  x : (replaceLettersInIban xs)
                              else (replaceLetter x) : (replaceLettersInIban xs)  

listToInt :: [Integer] -> Integer
listToInt [] = 0
listToInt l = if last l > 10 
              then (last l) + 100 * listToInt (init l)
              else (last l) + 10 * listToInt (init l)

checkIbanMod:: Integer -> Bool
checkIbanMod iban = (mod iban 97) == 1

-- Problem with Int && Integer but it is fine

-- iban:: String -> Bool 
-- iban = checkIbanMod . listToInt . replaceLettersInIban . moveBack

-- Tests: Source Tom 
-- ibanTestSet, ibanWrongTestSet :: [String]
-- ibanTestSet = ["AT483200000012345864", "NL79INGB0001611514", "BH02CITI00001077181611", "BG18RZBB91550123456789", "HR1723600001101234565", "DE91100000000123456789", "GB82WEST12345698765432", "GL8964710123456789", "LU120010001234567891", "ES7921000813610123456789"]
-- ibanWrongTestSet = ["AT403200000012345862", "NL79INGB0001611515", "BH02CITI00001075181611", "BG18RZBB91550123456788", "HR1723600001101233565", "DE91100000000123256789", "GB82WEST12345698764432", "GL8964710123456889", "LU120010001224567891", "ES7921000813611123456789"]
-- testIban, testWrongIban :: Bool
-- testIban = foldr (&&) True (map iban ibanTestSet) -- Test iban for all elements in the test set
-- testWrongIban = foldr (&&) True (map (not . iban) ibanWrongTestSet)

-- Bindings needed

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




      