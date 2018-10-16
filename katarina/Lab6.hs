module Lab6 where
import Data.List
import Lecture6
import Test.QuickCheck


-- ================= Ex 1 ==============

-- Input x^y mod modulo

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

--tests for exM
--exercise1Tests :: IO ()
exercise1Tests = do
    let res = [ (x,y,b) | x <- [1..20], y <- [1..20], b <- [5..20], exM x y b /= expM x y b]
    print "Testing implementation of exM via deterministic tests"
    print $ null res
    print "Testing implementation of exM via QuickCheck"
    quickCheckResult(\(x,y) -> x > 2 && y >= 1 --> exM x y 10 == expM x y 10)


-- =======Exercise 3=======
naturalNumbers = [1..]
composites = filter (\x -> not (prime x)) naturalNumbers 
-- ========================


-- =======Exercise 4========
fools k = findfools k composites
findfools k (x:xs) = do 
                    isPrime <- primeTestsF k x
                    if isPrime then return x else findfools k xs
-- all primes are considered as primes. But sometimes non-primes are considered as primes. Find them

-- fools 2 = 66
-- fools 3 = 451
-- fools 4 = 133
-- =========================



