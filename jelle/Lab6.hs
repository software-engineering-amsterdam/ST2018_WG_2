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
    print "Test Results for the primeTestsF function:"
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

