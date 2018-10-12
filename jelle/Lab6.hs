module Lab6

where

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

