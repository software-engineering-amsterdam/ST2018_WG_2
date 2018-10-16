module Lab6

where

import Lecture6
import System.IO.Unsafe
import System.Random
import Test.QuickCheck


infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- =======================
-- ==  1: Faster exM  ==
-- ==  Time: 2 hours; debugging for odd powers
-- =======================

-- See the implementation of exM in Lecture6.hs


--tests for exM
exercise1Tests :: IO ()
exercise1Tests = do
    let res = [ (x,y,b) | x <- [1..20], y <- [1..20], b <- [5..20], exM x y b /= expM x y b]
    print "Testing implementation of exM via deterministic tests"
    print $ null res
    print "Testing implementation of exM via QuickCheck"
    quickCheckResult(\(x,y) -> x > 2 && y >= 1 --> exM x y 10 == expM x y 10)
    return ()

-- =======================
-- ==  2: Faster exM  == 
-- == Time: 1 hour, finding libray for benchamrks and reading documentation
-- =======================

-- see file Ex2Benchmark.hs

-- =======================
-- ==  3: Composites  ==   30 minutes
-- =======================

-- An Integer is a composite if it occurs in the natural numbers and it is not a prime.
composites :: [Integer]
composites = filter (not . prime) [1..]


-- =========================
-- ==  4: Fermat Fails  ==   40 minutes
-- =========================

{-  Result:
*Lab6> exercise4Tests 
"Smallest composite number fooling fermat test with k checks"
"k = 2 ==> 861"
"k = 3 ==> 1105"
"k = 4 ==> 1105"

We see that as k increases, the smallest counterexample that slips through the Fermat test becomes larger. This is to be expected since a larger number of tests also decreases the likelihood of finding a counterexample, which makes the average smallest counterexample that we find likely to be larger than originally.
-}

-- Carmichael number generator
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
       k <- [2..], 
       prime (6*k+1), 
       prime (12*k+1), 
       prime (18*k+1) ]

-- helper function that takes a function to check numbers with and produces the smalles counterexample it can find. 
--We use this function throughout our answers to exercises 4, 5, and 6.
findSmallestCounterExample :: (Int -> Integer -> IO Bool) ->  Int -> [Integer] -> IO Integer
findSmallestCounterExample fun k (x:xs) = do 
    result <- fun k x
    if result then return x else findSmallestCounterExample fun k xs

counterExampleResult :: (Int -> Integer -> IO Bool) -> Int -> [Integer] -> IO ()
counterExampleResult fun k list = do
    result <- findSmallestCounterExample fun k list
    print $ "k = " ++ (show k) ++ " ==> " ++ (show result)

-- actual tests for exercise 4:
exercise4Tests :: IO ()
exercise4Tests = do
    print "Smallest composite number fooling fermat test with k checks"
    counterExampleResult primeTestsF 2 composites
    counterExampleResult primeTestsF 3 composites
    counterExampleResult primeTestsF 4 composites


-- ====================================================
-- ==  5: Fermat Fails using Carmichael numbers:  ==   40 minutes
-- ====================================================
{-  Result:
*Lab6> exercise5Tests 
"Smallest Carmichael number fooling Fermat test with k checks"
"k = 1 ==> 294409"
"k = 2 ==> 294409"
"k = 3 ==> 56052361"
-}
exercise5Tests :: IO ()
exercise5Tests = do
    print "Smallest Carmichael number fooling Fermat test with k checks"
    counterExampleResult primeTestsF 1 carmichael
    counterExampleResult primeTestsF 2 carmichael
    counterExampleResult primeTestsF 3 carmichael

-- ==========================================================
-- ==  6-1: Miller-Rabin Fails using Carmichael numbers:  ==   40 minutes
-- ==========================================================
{-  Result:
"Smallest Carmichael number fooling MR test with k checks"
"k = 1 ==> 216821881"
"k = 2 ==> 3719466204049"
"k = 3 ==> 15021804274836409"
-}

exercise6Tests :: IO ()
exercise6Tests = do
    print "Smallest Carmichael number fooling MR test with k checks"
    counterExampleResult primeMR 1 carmichael -- =  9
    counterExampleResult primeMR 2 carmichael
    counterExampleResult primeMR 3 carmichael

-- TODO: Insert explanation

-- ====================================
-- ==  6-2: MR-Mersenne Generator  ==   40 minutes
-- ====================================

millerRabinMersenneGen :: [Integer]
millerRabinMersenneGen = [2^p-1 | p <- primes, unsafePerformIO $ primeMR 2 (2^p - 1)]


mrMersenneTest :: Integer -> Int -> IO Bool
mrMersenneTest primeNum k  = primeMR k (2 ^ primeNum -1)

--alternative solution with monads, can not work with infinite lists
--generates a list of potential mersenne primes for given list
--uses miller rabin test to classify primality
mrMersennePrimeGenerator :: [Integer] -> IO [Integer]
mrMersennePrimeGenerator [] = return []
mrMersennePrimeGenerator (x:xs) = do 
    result <- mrMersenneTest x 1
    recurse <- mrMersennePrimeGenerator xs
    if result then return (x : recurse ) 
        else return recurse

mrMersennePrimeEvaluator :: IO ()
mrMersennePrimeEvaluator = do 
    result <- mrMersennePrimeGenerator (take 10000 primes)
    let wronglyClassified = filter (\z -> (not (elem z  [mers x | x <- [1..25]]))) result
    print "MR test to find Mersenne primes found the following non-Mersenne pseudoprimes:"
    print wronglyClassified

{-
mrMersennePrimeGenerator (take 1000 primes )
[2,3,5,7,13,17,19,31,61,89,107,127,521,607,1279,2203,2281,3217,4253,4423]
-}


{-  Result:
*Lab6> take 12 millerRabinMersenneGen 
[3, 7, 31, 127, 8191, 131071, 524287, 2147483647, 2305843009213693951, 618970019642690137449562111, 162259276829213363391578010288127, 170141183460469231731687303715884105727]

The actual first twelve Mersenne primes are:
(source: https://oeis.org/A000668)
[3, 7, 31, 127, 8191, 131071, 524287, 2147483647, 2305843009213693951, 618970019642690137449562111, 162259276829213363391578010288127, 170141183460469231731687303715884105727]
-}

-- TODO JM: Insert  explanation

-- TODO JM: Make up times