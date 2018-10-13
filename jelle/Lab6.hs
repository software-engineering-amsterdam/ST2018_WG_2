module Lab6

where

import System.IO.Unsafe
import Lecture6

highestPowerOfTwo :: Int -> Int
highestPowerOfTwo n = maximum [x | x <- [1..n], elem x [2^y | y <- [1..n]]]


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

fermatFailers :: Int -> [Integer]
fermatFailers k = [x | x <- composites, unsafePerformIO $ primeTestsF k x]

fermatTestReport = do
    let results = [head $ fermatFailers x | x <- [1..6]]
    print "Test Results for the primeTestsF function using Composites:"
    print "+---+------------"
    print "| k | First Fail "
    print "+---+------------"
    print ("| 1 | " ++ (show (results !! 0)))
    print ("| 2 | " ++ (show (results !! 1)))
    print ("| 3 | " ++ (show (results !! 2)))
    print ("| 4 | " ++ (show (results !! 3)))
    print ("| 5 | " ++ (show (results !! 4)))
    print ("| 6 | " ++ (show (results !! 5)))
    print "+---+------------"

{-  Result:
*Lab6> fermatTestReport 
"Test Results for the primeTestsF function:"
"+---+------------"
"| k | First Fail "
"+---+------------"
"| 1 | 35"
"| 2 | 703"
"| 3 | 561"
"| 4 | 1105"
"| 5 | 1105"
"| 6 | 1729"
"+---+------------"
-}

-- ===================================
-- ==  5: Carmichael Fermat Test  ==   40 minutes
-- ===================================

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
    k <- [2..], 
    prime (6*k+1), 
    prime (12*k+1), 
    prime (18*k+1) ]

fermatCarmichaelFailers :: Int -> [Integer]
fermatCarmichaelFailers k = [x | x <- carmichael, unsafePerformIO $ primeTestsF k x]

fermatCarmichaelTestReport = do
    let results = [head $ fermatCarmichaelFailers x | x <- [1..6]]
    print "Test Results for the primeTestsF function using Carmichael:"
    print "+---+------------"
    print "| k | First Fail "
    print "+---+------------"
    print ("| 1 | " ++ (show (results !! 0)))
    print ("| 2 | " ++ (show (results !! 1)))
    print ("| 3 | " ++ (show (results !! 2)))
    print ("| 4 | " ++ (show (results !! 3)))
    print ("| 5 | " ++ (show (results !! 4)))
    print ("| 6 | " ++ (show (results !! 5)))
    print "+---+------------"

{-  Result:
*Lab6> fermatCarmichaelTestReport 
"Test Results for the primeTestsF function using Carmichael:"
"+---+------------"
"| k | First Fail "
"+---+------------"
"| 1 | 294409"
"| 2 | 56052361"
"| 3 | 56052361"
"| 4 | 294409"
"| 5 | 294409"
"| 6 | 294409"
"+---+------------"
-}

-- =====================================
-- ==  6-1: Miller Rabin Primality  ==   40 minutes
-- =====================================

millerRabinFailers :: Int -> [Integer]
millerRabinFailers k = [x | x <- composites, unsafePerformIO $ primeMR k x]

millerRabinTestReport = do
    let results = [head $ millerRabinFailers x | x <- [1..6]]
    print "Test Results for the primeMR function using Composites:"
    print "+---+------------"
    print "| k | First Fail "
    print "+---+------------"
    print ("| 1 | " ++ (show (results !! 0)))
    print ("| 2 | " ++ (show (results !! 1)))
    print ("| 3 | " ++ (show (results !! 2)))
    print ("| 4 | " ++ (show (results !! 3)))
    print ("| 5 | " ++ (show (results !! 4)))
    print ("| 6 | " ++ (show (results !! 5)))
    print "+---+------------"

{-  Result:
*Lab6> millerRabinTestReport 
"Test Results for the primeMR function using Composites:"
"+---+------------"
"| k | First Fail "
"+---+------------"
"| 1 | 172"
"| 2 | 12403"
"| 3 | 4033"
"| 4 | ^CInterrupted. -> Took too damn long
-}

-- ====================================
-- ==  6-2: MR-Mersenne Generator  ==   40 minutes
-- ====================================

millerRabinMersenneGen :: [Integer]
millerRabinMersenneGen = [2^p-1 | p <- primes, unsafePerformIO $ primeMR 2 (2^p - 1)]

{-  Result:
*Lab6> take 7 millerRabinMersenneGen 
[3, 7, 31, 127, 8191, 131071, 524287]

The actual first seven Mersenne primes are:
(source: https://oeis.org/A000668)
[3, 7, 31, 127, 8191, 131071, 524287]

Asking the generator for any more overloaded my RAM. It seems the Miller-Rabinson primlity test is strong enough to legitimitely find the first seven Mersenne primes. The question of whether this holds up for larger primes can only be answered by  machine more powerful than my BYOD laptop.
-}