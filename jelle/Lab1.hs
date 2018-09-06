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

reversal :: Integer -> Integer
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


-- =========
-- === 4 ===
-- =========


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


-- =========
-- === 7 ===
-- =========
intToList :: Integer -> [Integer]
intToList 0 = []
intToList n = (intToList (quot n 10)) ++ [mod n 10]

listToInt :: [Integer] -> Integer
listToInt [] = 0
listToInt l = 
    (last l) + 10 * listToInt (init l)


odds :: [Integer] -> Bool -> [Integer]
odds [] _ = []
odds list thisOne = if thisOne
    then last list : odds (init list) False
    else odds (init list) True

specialDouble :: [Integer] -> [Integer]
specialDouble [] = []
specialDouble (x:xs) = 
    if x+x > 9 
        then (x+x-9) : (specialDouble xs)
        else (x+x) : (specialDouble xs)

luhn :: Integer -> Bool
luhn n = 
    let nList = intToList n 
        list1 = odds nList False
        list2 = odds nList True
        total = sum ((specialDouble list1) ++ list2)
    in if mod total 10 == 0
        then True else False

isAmericanExpress :: Integer -> Bool
isAmericanExpress n = 
    let l = intToList n
        firstTwo = listToInt (take 2 l)
    in 
    if  (length l == 15) &&
        (firstTwo == 34 || firstTwo == 37) &&
        (luhn n) then True else False

isMaster :: Integer -> Bool
isMaster n =
    let l = intToList n
        firstTwo = listToInt (take 2 l)
        firstSix = listToInt (take 6 l)
    in
    if  (length l == 16) &&
        ((firstSix >= 222100 && firstSix <= 272099) ||
            (elem firstTwo [51..55])) &&
        (luhn n) then True else False

isVisa :: Integer -> Bool
isVisa n = 
    if  (elem (length (intToList n)) [13,16,19]) &&
        ((head (intToList n)) == 4) &&
        (luhn n) then True else False

