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



--exercise 1, workshop problem 2

ex1P2Predicate :: Integer -> Bool
ex1P2Predicate n = (sum $ map (^2) [1..n]) == (n*(n+1)*(2*n-1) `div` 6)



--exercise 2
{-

Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually? Are you checking a mathematical fact? Or are you testing whether subsequences satisfies a part of its specification? Or are you testing something else still?

In Haskell we can only check lists up to a finite size limited by memory / stack etc, while formal mathematical proofs can be used to prove a statement for the entire domain (including infinite domains).


statement: |A| = n, then |superset(A)| = 2^n

generate superset by subsets command. 

time: 15 minutes 

-}


generateSetOfLength len = [1..len]
superSetLength xs = length( subsequences xs )

ex2PropertyHolds :: Integer -> Bool
ex2PropertyHolds len = 2 ^ len == superSetLength ( generateSetOfLength len )


{-

The natural number 13 has the property that it is prime and its reversal, the number 31, is also prime. Write a function that finds all primes < 10000 with this property.

To get you started, here is a function for finding the reversal of a natural number:

> reversal :: Integer -> Integer
> reversal = read . reverse . show
How would you test this function, by the way?

time: 40min
-}



ex4NumSameAsReversePrime :: Integer -> Bool
ex4NumSameAsReversePrime x = (prime x) && (prime (reversal x))
ex4NumGenerate :: [Integer]
ex4NumGenerate = filter ex4NumSameAsReversePrime (filter prime [1..10000-1] ) 

--exercise 5
--time: 49 mnins

sumOfConsecutivePrimes startIndex howMany = sum ( take howMany (snd ( splitAt (startIndex) primes) ))
isSumPrime startIndex howMany =  prime (sumOfConsecutivePrimes startIndex howMany)
ex5FindSmallestSumPrimeIndex = filter (\startIndex -> isSumPrime startIndex 101  ) [1..]
ex5FindSmallestSumPrime = sumOfConsecutivePrimes (ex5FindSmallestSumPrimeIndex !! 0) 101

--exercise 6
-- time: 26min

primeProduct n = foldl (*) 1 (take n primes)
ex6Predicate n = prime ((primeProduct n) + 1)
ex6Result = (filter (not . ex6Predicate) [1..] ) !! 0 --to generate more remove the !!
ex6Result' = (filter (\x -> not (ex6Predicate x)) [1..] ) !! 0

--exercise 7
--time 1 hour, 15minutes

--source: https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell#
digits :: Integer -> [Int]
digits = map (read . return) . show

luhn :: Integer -> Bool
luhn number = luhnSum number `mod` 10 == 0

luhnSum number =  sum $ luhnDoubleList $ reverse $ digits number

luhnDouble x = if x*2 > 9 then x*2 - 9 else x*2

luhnDoubleList [] = []
luhnDoubleList (a:[]) = a:[] --TODO check algo
luhnDoubleList (a:b:rest) = a:luhnDouble b : luhnDoubleList rest


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

