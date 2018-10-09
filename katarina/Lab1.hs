
module Lab1 where
import Data.List
import Test.QuickCheck    

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-------------------------------------------------------------------------------------------
--Katarina Lang--
-------------------------------------------------------------------------------------------

-- Exercise 1

    -- Problem 2 

sumPot2 n = sum (map (\x -> x^2) [1..n])
sumPot2' n = quot (n*(n+1)*(2*n+1)) 6

checkSumPot2 n = sumPot2 (abs n) == sumPot2' (abs n)
    -- abs because the statements only holds for natural numbers
 
   --Problem 3

sumPot3 n = sum (map (\x -> x^3) [1..n])
sumPot3' n = (quot (n*(n+1)) 2)^2

checkSumPot3 n = sumPot3 (abs n) == sumPot3' (abs n)



-- Exercise 2

sizeOfPowerSet n = length (subsequences [1..n])
sizeOfPowerSet' n = 2^n

formularForListsHolds n = sizeOfPowerSet(abs n) == sizeOfPowerSet' (abs n)

-- This formular is hard to test, because it should hold for every number that is greater than 0. It is not possible to check this statement for an infinite number of numbers in a finite time.
-- Quick Check only puts some numbers and checks if it is right, not a proof.



-- ======Ex 3=====

-- idea n! is the number of reorderings

faculty::Int->Int
faculty 0 = 1
faculty n = n*(faculty (n-1))

quickCheckPerms :: Int -> Bool
quickCheckPerms n = (length $ permutations [1..(mod (abs n) 5)]) == faculty (mod (abs n) 5)

-- ===============

-- Exercise 4

first1000Primes = filter prime [1..9999]
isPrimeAndReversalPrime n = prime n && prime (reversal n)

first1000ReversalPrimes = filter isPrimeAndReversalPrime first1000Primes
first1000ReversalPrimes' = filter (\x -> prime x && prime (reversal x)) first1000Primes

   --And in one line:
first1000ReversalPrimes'' = filter (\x -> prime x && prime (reversal x)) [1..9999]
   -- how to test: is every number a prime? And is the reversal in our list?



---Exercise 5

consecutivePrimes101 n = sum (take 101 $ filter (\x -> x>=n) primes)

resultEx5 = head $ filter prime (map consecutivePrimes101 [1..])

-- ======= Ex 6=====
counterexampleGen::Int->Int
counterexampleGen n = if not $ prime ((foldr (*) 1 (take n primes))+1)
                      then (foldr (*) 1 (take n primes))+1
                      else counterexampleGen (n+1)

-- ================



-- Exercise 7

digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n) --n is number of credit card, digits returns [1,2,3,4]

lastDigit creditcardNumber = last (digits creditcardNumber)

reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

second (x:y:xs) = y : second xs
second _ = []

first (x:y:xs) = x:first xs
first (x:[]) = [x]
first [] = []

multiplyEveryoneByTwo :: [Int] -> [Int]
multiplyEveryoneByTwo [] = []
multiplyEveryoneByTwo (x:xs) = luhnDouble x : multiplyEveryoneByTwo xs

luhnDouble x = if((x*2) > 9) then ((x*2)-9) else (x*2)

luhnSum creditcardNumber = sum $ multiplyEveryoneByTwo(second(reverseList(digits creditcardNumber))) ++ first(reverseList(digits creditcardNumber))
luhnCorrect creditcardNumber = (luhnSum creditcardNumber) `mod` 10 == 0





-- Exercise 8




