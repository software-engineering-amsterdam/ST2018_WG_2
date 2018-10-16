module Lab6

where

import Lecture6
import System.IO.Unsafe
import System.Random
import Test.QuickCheck

-- =======================
-- ==  1: Faster exM  ==
-- =======================

-- See The implementation of exM in Lecture6
infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

exercise1Tests = do
    let res = [ (x,y,b) | x <- [1..20], y <- [1..20], b <- [5..20], exM x y b /= expM x y b]
    print "Testing implementation of exM via deterministic tests"
    print $ null res
    print "Testing implementation of exM via QuickCheck"
    quickCheckResult(\(x,y) -> x > 2 && y >= 1 --> exM x y 10 == expM x y 10)

-- =======================
-- ==  2: Faster exM  ==
-- =======================

-- see file Ex2Benchmark.hs

-- =======================
-- ==  3: Composites  ==   20 minutes
-- =======================

-- An Integer is a composite if it occurs in the natural numbers, starting at 4, and it is not a prime.
composites :: [Integer]
composites = [x | x <- [4..], x `notIn` primes]

-- This element checker assumes the provided list is ordered, as is the case with the primes list. If this is the case, we can stop checking for occurences if we have passed the point where the sought number is smaller than the currently checking number. If it were in the list, we would already have found it at this point.
notIn :: Integer -> [Integer] -> Bool
notIn n (x:xs)
    | n < x = True
    | n == x = False
    | otherwise = notIn n xs

-- =========================
-- ==  4: Fermat Fails  ==   40 minutes
-- =========================

-- Cramichael number generator
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
       k <- [2..], 
       prime (6*k+1), 
       prime (12*k+1), 
       prime (18*k+1) ]

-- helper function that takes a function to check numbers with and produces the smalles counterexample it can find. We use this function throughout our answers to exercises 4, 5, and 6.
findSmallestCounterExample :: (Int -> Integer -> IO Bool) ->  Int -> [Integer] -> IO Integer
findSmallestCounterExample fun k (x:xs) = do 
    result <- fun k x
    if result then return x else findSmallestCounterExample fun k xs

exercise46SingleResult fun k list = do
    result <- findSmallestCounterExample fun k list
    print $ "k = " ++ (show k) ++ " ==> " ++ (show result)

-- actual tests for exercise 4:
exercise4Tests = do
    print "Smallest composite number fooling fermat test with k checks"
    exercise46SingleResult primeTestsF 2 composites
    exercise46SingleResult primeTestsF 3 composites
    exercise46SingleResult primeTestsF 4 composites
   
-- ====================================================
-- ==  5: Fermat Fails using Carmichael numbers:  ==   40 minutes
-- ====================================================
exercise5Tests = do
    print "Smallest carmichael number fooling Fermat test with k checks"
    exercise46SingleResult primeTestsF 1 carmichael
    exercise46SingleResult primeTestsF 2 carmichael
    exercise46SingleResult primeTestsF 3 carmichael

-- ==========================================================
-- ==  6: Miller-Rabin Fails using Carmichael numbers:  ==   40 minutes
-- ==========================================================
exercise6Tests = do
    print "Smallest composite number fooling MR test with k checks"
    exercise46SingleResult primeMR 1 carmichael -- =  9
    exercise46SingleResult primeMR 2 carmichael
    exercise46SingleResult primeMR 3 carmichael

-- ====================================
-- ==  6-2: MR-Mersenne Generator  ==   40 minutes
-- ====================================

millerRabinMersenneGen :: [Integer]
millerRabinMersenneGen = [2^p-1 | p <- primes, unsafePerformIO $ primeMR 2 (2^p - 1)]

{-  Result:
*Lab6> take 12 millerRabinMersenneGen 
[3, 7, 31, 127, 8191, 131071, 524287, 2147483647, 2305843009213693951, 618970019642690137449562111, 162259276829213363391578010288127, 170141183460469231731687303715884105727]

The actual first twelve Mersenne primes are:
(source: https://oeis.org/A000668)
[3, 7, 31, 127, 8191, 131071, 524287, 2147483647, 2305843009213693951, 618970019642690137449562111, 162259276829213363391578010288127, 170141183460469231731687303715884105727]
-}

-- TODO: Provide alternative solution without unsafePerformIO