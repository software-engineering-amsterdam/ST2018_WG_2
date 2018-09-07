-- ===================
--  Assignemnt givens
-- ===================
module Lab1 where
import Data.List
import Numeric
import Test.QuickCheck    

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Int]
primes = 2 : filter prime [3,5..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Int -> Int
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- =========
-- === 1 ===
-- =========

-- quadSum definition, using quot because the / operator
-- makes Haskell whine about Fractionals
quadSum :: Int -> Int
quadSum n = quot (n*(n+1)*(2*n+1)) 6

quadSum' :: Int -> Int
quadSum' n = sum (map (\x -> x^2) [1..n])

-- tripSum definition, also using quot instead of /
tripSum :: Int -> Int
tripSum n = (quot (n * (n+1)) 2)^2

tripSum' :: Int -> Int
tripSum' n = sum (map (\x -> x^3) [1..n])

-- quickCheck test definitions
quadSumTest x = quadSum (abs x) == quadSum' (abs x)
tripSumTest y = tripSum (abs y) == tripSum' (abs y)

-- =========
-- === 2 ===
-- =========

-- =========
-- === 3 ===
-- =========
count :: Int -> [Int] -> Int
count x = length . filter (==x)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n-1)

permCalc :: [Int] -> Int
permCalc l = quot (fact (length l)) (instanceFact l)

instanceFact :: [Int] -> Int
instanceFact l = foldl (*) 1 (map (\x -> fact(x)) (instanceCount l))

instanceCount :: [Int] -> [Int]
instanceCount l = instanceCount' [] l

instanceCount' :: [Int] -> [Int] -> [Int]
instanceCount' l [] = l
instanceCount' counts instances = 
    let thisInstance = head instances
        thisInstanceCount = count thisInstance instances
        nextCounts = thisInstanceCount:counts
        nextInstances = filter (/=thisInstance) instances
    in instanceCount' nextCounts nextInstances

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)


-- =========
-- === 4 ===
-- =========
revPrimes :: [Int]
revPrimes = filter (\x -> (prime x) && (prime (reversal x))) [1..10000]

-- =========
-- === 5 ===
-- =========

consPrimes :: Int -> [Int]
consPrimes n = consPrimes' n primes

consPrimes' :: Int -> [Int] -> [Int]
consPrimes' n primeList = 
    if prime (sum (take n primeList))
        then sum (take n primeList) : consPrimes' n (tail primeList)
        else consPrimes' n (tail primeList)

-- =========
-- === 6 ===
-- =========
conjCounter :: [Int]
conjCounter = conjCounter' 1

conjCounter' :: Int -> [Int]
conjCounter' n =
    if (not (prime ((foldr (*) 1 (take n primes)) + 1)))
        then ((foldr (*) 1 (take n primes)) + 1) : conjCounter' (n+1)
        else conjCounter' (n+1)

-- =========
-- === 7 ===
-- =========
intToList :: Int -> [Int]
intToList 0 = []
intToList n = (intToList (quot n 10)) ++ [mod n 10]

listToInt :: [Int] -> Int
listToInt [] = 0
listToInt l = 
    (last l) + 10 * listToInt (init l)


odds :: [Int] -> Bool -> [Int]
odds [] _ = []
odds list thisOne = if thisOne
    then last list : odds (init list) False
    else odds (init list) True

specialDouble :: [Int] -> [Int]
specialDouble [] = []
specialDouble (x:xs) = 
    if x+x > 9 
        then (x+x-9) : (specialDouble xs)
        else (x+x) : (specialDouble xs)

luhn :: Int -> Bool
luhn n = 
    let nList = intToList n 
        list1 = odds nList False
        list2 = odds nList True
        total = sum ((specialDouble list1) ++ list2)
    in if mod total 10 == 0
        then True else False

isAmericanExpress :: Int -> Bool
isAmericanExpress n = 
    let l = intToList n
        firstTwo = listToInt (take 2 l)
    in 
    if  (length l == 15) &&
        (firstTwo == 34 || firstTwo == 37) &&
        (luhn n) then True else False

isMaster :: Int -> Bool
isMaster n =
    let l = intToList n
        firstTwo = listToInt (take 2 l)
        firstSix = listToInt (take 6 l)
    in
    if  (length l == 16) &&
        ((firstSix >= 222100 && firstSix <= 272099) ||
            (elem firstTwo [51..55])) &&
        (luhn n) then True else False

isVisa :: Int -> Bool
isVisa n = 
    if  (elem (length (intToList n)) [13,16,19]) &&
        ((head (intToList n)) == 4) &&
        (luhn n) then True else False

-- =========
-- === 8 ===
-- =========

-- accuses :: Boy -> Boy -> Bool
combinations :: Eq a => [a] -> [[a]]
combinations l = rmDups (perms l)

rmDups :: Eq a => [[a]] -> [[a]]
rmDups [] = []
rmDups (x:xs) = if elem x xs
    then rmDups xs else x:(rmDups xs)

guiltyOptions = combinations [True, False, False, False, False]
truthOptions = combinations [True, True, True, False, False]

-- ===============
-- === EULER 9 ===
-- ===============
pythTrips :: [(Int, Int, Int)]
pythTrips = [(x,y,z) | x <- [1..500], y <- [x..500], z <- [y..500], x^2 + y^2 == z^2]

specPythTripProd :: Int
specPythTripProd = 
    let (a,b,c) = specPythTrip' pythTrips
    in a*b*c

specPythTrip' :: [(Int, Int, Int)] -> (Int, Int, Int)
specPythTrip' ((a,b,c):xs) = if a + b + c == 1000
    then (a,b,c) else specPythTrip' xs

-- ================
-- === EULER 10 ===
-- ================
sieve :: [Int] -> [Int]
sieve [] = []
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)

sumOfTwoMilPrimes :: Int
sumOfTwoMilPrimes = sumOfTwoMilPrimes' (sieve (2:[3,5..])) 0

sumOfTwoMilPrimes' :: [Int] -> Int -> Int
sumOfTwoMilPrimes' (x:xs) n = 
    if x >= 2000000 then n
        else sumOfTwoMilPrimes' xs n+x

-- ================
-- === EULER 49 ===
-- ================
fourDigitPrimes = sieve [1001,1003..9999]

permutation :: Int -> Int -> Bool
permutation x y = 
    let x = listToInt (sort (intToList x))
        y = listToInt (sort (intToList y))
    in x == y

seqPermPrimes :: (Int, Int, Int)
seqPermPrimes = 
