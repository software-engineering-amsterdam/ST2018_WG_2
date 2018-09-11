module Lab1 where
import Data.List
import Test.QuickCheck
import 

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


--quickcheck positive number generator
--https://stackoverflow.com/questions/39291494/only-generate-positive-integers-with-quickcheck
{-
genPos :: Gen Int
genPos = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

genListOfPos :: Gen [Int]
genListOfPos = listOf genPos
-}



--exercise 1, workshop problem 2

ex1QuadSum n = sum $ map (^2) [1..n]
ex1QuadSum' n = n*(n+1)*(2*n+1) `div` 6

ex1P2Predicate :: Integer -> Bool
ex1P2Predicate n =  ex1QuadSum (abs n) == ex1QuadSum' (abs n)

--tests:
--quickCheck ex1P2Predicate


--exercise 1, workshop problem 3

ex1CubeSum n = sum $ map (^3) [1..n]
ex1CubeSum' n = (n*(n+1) `div` 2) ^2

ex1P3Predicate :: Integer -> Bool
ex1P3Predicate n =  ex1CubeSum (abs n) == ex1CubeSum' (abs n)

--tests:
--quickCheck ex1P3Predicate


--exercise 2
{-

Give your thoughts on the following issue: when you perform the test for exercise 4, 
what are you testing actually? Are you checking a mathematical fact? Or are you 
testing whether subsequences satisfies a part of its specification? Or are you 
testing something else still?


Statement: |A| = n, then |superset(A)| = 2^n

generate superset by subsets command. 


Answer: In Haskell we can only check lists up to a finite size limited by 
memory / stack etc, while formal mathematical proofs can be used to prove a 
statement for the entire domain (including infinite domains).


time: 20 minutes 

-}


generateSetOfLength len = [1..len]
superSetLength xs = length( subsequences xs )


ex2Property len = superSetLength ( generateSetOfLength len )
ex2Property' len = 2 ^ len 

ex2Predicate len = ex2Property len == ex2Property' len

--tests:
--quickCheck ex2Predicate


{-

The natural number 13 has the property that it is prime and its reversal, the number 31, is also prime. Write a function that finds all primes < 10000 with this property.

To get you started, here is a function for finding the reversal of a natural number:

> reversal :: Integer -> Integer
> reversal = read . reverse . show
How would you test this function, by the way?


Answer: 
1. test the ex4NumSameAsReversePrime function that it outputs True only IFF input is prime and the reverse is prime
2. test ex4NumGenerate that each element in the list satisfies the ex4NumSameAsReversePrime condition

time: 40min
-}


ex4NumSameAsReversePrime :: Integer -> Bool
ex4NumSameAsReversePrime x = (prime x) && (prime (reversal x))
ex4NumGenerate :: [Integer]
ex4NumGenerate = filter ex4NumSameAsReversePrime (filter prime [1..10000-1] ) 



--exercise 5
--time: 55 mnins

{-
Do you have to test that your answer is correct? How could this be checked?

Answer: ex5FindSmallestSumPrimeIndex finds all primes such that the sum of 101 consecutive primes is also prime

For testing, we could write test cases to verify that eg: 5 consecutive primes are exactly the ones in the instructions
-}

sumOfConsecutivePrimes startIndex howMany = sum ( take howMany (snd ( splitAt (startIndex) primes) ))
isSumPrime startIndex howMany =  prime (sumOfConsecutivePrimes startIndex howMany)
ex5FindSmallestSumPrimeIndex = filter (\startIndex -> isSumPrime startIndex 101  ) [1..]
ex5FindSmallestSumPrime = sumOfConsecutivePrimes (ex5FindSmallestSumPrimeIndex !! 0) 101

--exercise 6
-- time: 26min
--answer: 6

primeProduct n = foldl (*) 1 (take n primes)
ex6Predicate n = prime ((primeProduct n) + 1)
ex6Result = (filter (not . ex6Predicate) [1..] ) !! 0 --to generate more remove the !!


--exercise 7
--time 1 hour, 15minutes

{-
Finally, design and implement a test for correctness of your implementation.

Answer:

-}

--source: https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell#
digits :: Integer -> [Int]
digits = map (read . return) . show

luhn :: Integer -> Bool
luhn number = luhnSum number `mod` 10 == 0

luhnSum number =  sum $ luhnDoubleList $ reverse $ digits number

luhnDouble x = if x*2 > 9 then x*2 - 9 else x*2

luhnDoubleList [] = []
luhnDoubleList (a:[]) = a:[]
luhnDoubleList (a:b:rest) = a:luhnDouble b : luhnDoubleList rest

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress number = (digits number) !! 0 == 3 && (digits number) !! 1 `elem` [4,7]  && luhn number
isMaster number = (digits number) !! 0 == 5 && (digits number) !! 1 `elem` [1..5]  && luhn number
isVisa number = (digits number) !! 0 == 4 && luhn number


--exercise 8
--time: 41 mins, not done

accuses Matthew Peter = True
accuses Matthew Jack = True
accuses Matthew Arnold = True

accuses Peter Matthew = True
accuses Peter Jack = True

accuses Jack Matthew  = True 
accuses Jack Carl = True
accuses Jack Peter = True
accuses Jack Arnold = True 


accuses Arnold Matthew = True
accuses Arnold Peter = True
accuses Arnold Carl = True
accuses Arnold Arnold = True

accuses Carl Jack = True

accuses _ _ = False


accusers :: Boy -> [Boy]
accusers boyAccused = filter (\accuser -> accuses accuser boyAccused) boys

boyGuiltyPredicate boy = length (accusers boy) == 3




