
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


-- Exercise 1 Are the numbers generated randomly?
--areInTheFirstQuarter :: 
randomNumbers n = probs n

areInTheFirstQuartile = filter (\x -> x < 0.25 && x>0) 



---Exercise 3 First Part
-- Implement the properties
firstPropertyLeft, secondPropertyLeft, thirdPropertyLeft, fourthPropertyLeft :: Int -> Bool 
firstPropertyRight, secondPropertyRight, thirdPropertyRight, fourthPropertyRight :: Int -> Bool 


--firstPropertyLeft x = even x && x > 3  
--firstPropertyRight x = even x
--secondPropertyLeft x = even x || x > 3 
--secondPropertyRight x = even x
--thirdPropertyLeft x = even x && x > 3 || even x
--thirdPropertyRight x = even x
--fourthPropertyLeft x = even x 
--fourthPropertyRight x = even x && x > 3 || even x

firstPropertyLeft = (\x -> even x && x > 3)
firstPropertyRight = even
secondPropertyLeft  = (\x ->even x || x > 3 )
secondPropertyRight  = even
thirdPropertyLeft  = (\x -> even x && x > 3 || even x)
thirdPropertyRight  = even 
fourthPropertyLeft  = even 
fourthPropertyRight = (\x ->even x && x > 3 || even x)

--listOfAllProperties x = [firstPropertyLeft x, firstPropertyRight x, secondPropertyLeft x, secondPropertyRight x,thirdPropertyLeft x, thirdPropertyRight x, fourthPropertyLeft x, fourthPropertyRight x]
listOfAllProperties = [firstPropertyLeft , firstPropertyRight, secondPropertyLeft, secondPropertyRight ,thirdPropertyLeft, thirdPropertyRight, fourthPropertyLeft, fourthPropertyRight]
--Provide a list of descending strength
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

--quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
   quicksort [ a | a <- xs, weaker [-10..10] a x ]
   ++ [x]
   ++ quicksort [ a | a <- xs, stronger [-10..10] a x ]



--Exercie 3 Second Part
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) ys = if x `elem` ys then isPermutation (xs) (deleteOneOccurence ys [] x) else False

deleteOneOccurence [] res _ = res
deleteOneOccurence (x:xs) ys  f = if x == f then (ys++xs) else deleteOneOccurence xs (x:ys) f

--Test properties
equalLength a b = length a == length b
everyElementOfAInB a b = forall a (\x -> elem x b)
everyElementOfBInA a b = everyElementOfAInB b a

permutationTest :: (Eq a) => [a]->[a]->Bool
permutationTest a b = equalLength a b && everyElementOfAInB a b && everyElementOfBInA a b

permutationTest' = permutationTest [1,2,3,4,5] [5,3,4,2,1] && permutationTest [1,2,3] [3,2,1] && permutationTest [1] [1] 
  && permutationTest [1,2,3] [3,2,2,3]
--  && permutationTest [] [] 


-- For quick check we cant use the generator for both lists to compare. We will use the generator for one list and manipulate this list to compare. Two versions swap and reverse
swap2 :: [a]->[a]
swap2 [] = []
--swap2 (x:[]) = [x]
swap2 [x] = [x]
swap2 (x:y:xs)  = y:x:(swap2 xs)

reverseList :: [a] -> [a]
reverseList [] = []
reverseList [x] = [x]
reverseList (x:xs) = reverseList xs ++ [x]


-- We want to be sure that the function is wrong if we dont have a permutation. Therefor we test two lists of the same length but with different elements
quickCheckPermutationSwap a = permutationTest a (swap2 a) 
quickCheckPermutationReverse a = permutationTest a (reverseList a)
quickCheckPermutationNotEqual a = permutationTest a (map (\x -> x+10) a)