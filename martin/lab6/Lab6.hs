module Lab6 where
 
import Data.List
import System.Random
import Lecture6
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


exercise1Tests = do
    let res = [ (x,y,b) | x <- [1..20], y <- [1..20], b <- [5..20], exM x y b /= expM x y b]
    print "Testing implementation of exM via deterministic tests"
    print $ null res
    print "Testing implementation of exM via QuickCheck"
    quickCheckResult(\(x,y) -> x > 2 && y >= 1 --> exM x y 10 == expM x y 10)


nats :: [Integer]
nats = [1..]

composites :: [Integer]
composites = filter (not . prime) nats

findSmallestCounterExample :: (Int -> Integer -> IO Bool) ->  Int -> [Integer] -> IO Integer    
findSmallestCounterExample fun k (x:xs) = do 
    result <- fun k x
    if result then return x else findSmallestCounterExample fun k xs

exercise46SingleResult fun k list = do
    result <- findSmallestCounterExample fun k list
    print $ "k = " ++ (show k) ++ " ==> " ++ (show result)

exercise4Tests = do
    print "Smallest composite number fooling fermat test with k checks"
    exercise46SingleResult primeTestsF 1 composites
    exercise46SingleResult primeTestsF 2 composites
    exercise46SingleResult primeTestsF 3 composites
    exercise46SingleResult primeTestsF 4 composites
    
exercise5Tests = do
    print "Smallest carmichael number fooling Fermat test with k checks"
    exercise46SingleResult primeTestsF 1 carmichael
    exercise46SingleResult primeTestsF 2 carmichael
    exercise46SingleResult primeTestsF 3 carmichael

exercise6Tests = do
    print "Smallest carmichael number fooling MR test with k checks"
    exercise46SingleResult primeMR 1 carmichael
    exercise46SingleResult primeMR 2 carmichael
    exercise46SingleResult primeMR 3 carmichael



carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
       k <- [2..], 
       prime (6*k+1), 
       prime (12*k+1), 
       prime (18*k+1) ]


{-
You can use the Miller-Rabin primality check to discover some large Mersenne primes. The recipe: take a prime p, and use the Miller-Rabin algorithm to check whether 2^p −1 is also prime. Find information about Mersenne primes on internet and check whether the numbers that you found are genuine Mersenne primes. Report on your findings.
-}


mrMersenneTest :: Integer -> Int -> IO Bool
mrMersenneTest primeNum k  = primeMR k (2 ^ primeNum -1)


--generates a list of potential mersenne primes for given list
--uses miller rabin test to classify primality
mrMersennePrimeGenerator [] = return []
mrMersennePrimeGenerator (x:xs) = do 
    result <- mrMersenneTest x 1
    recurse <- mrMersennePrimeGenerator xs
    if result then return (x : recurse ) 
        else return recurse


mrMersennePrimeEvaluator = do 
    result <- mrMersennePrimeGenerator (take 10000 primes)
    let wronglyClassified = filter (\z -> (not (elem z  [mers x | x <- [1..25]]))) result
    print "MR test to find Mersenne primes found the following non-Mersenne pseudoprimes:"
    print wronglyClassified

{-
mrMersennePrimeGenerator (take 1000 primes )
[2,3,5,7,13,17,19,31,61,89,107,127,521,607,1279,2203,2281,3217,4253,4423]
-}
