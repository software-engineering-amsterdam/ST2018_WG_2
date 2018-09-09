import Data.List
import Numeric
import Data.Char

-- Task 1
regularSum :: Int -> Int
regularSum n = sum [1..n]

regularSum' n = (n*(n+1))/2

sumSquare :: Int -> Int
sumSquare n = 
    let list = [1..n]
        squaredList = [x^2 | x <- list] 
    in sum squaredList

sumSquare' n = n*((n+1)*(2*n+1))/6

-- Additional

sumTriple :: Int -> Int
sumTriple n = 
    let list = [1..n]
        squaredList = [x^3 | x <- list] 
    in sum squaredList

sumTriple' :: Int -> Int
sumTriple' n = (quot (n * (n+1)) 2)^2

-- Task 5

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Int]
primes = 2 : filter prime [3,5..] 

consPrimes :: Int -> Int
consPrimes n = sum (cPrimes n primes) -- sum the first consecutive numbers found  
		

cPrimes:: Int -> [Int] -> [Int]
cPrimes x primeList = 
		  if prime(sum(take x primeList)) -- checks if the sum of x elements is prime
		  then (take x primeList) -- returns the consecutive numbers, which sum is prime 
		  else cPrimes x (tail (primeList)) -- recursion with the rest of the numbers of primes


-- Task 7

intToList:: Int -> [Int]
intToList 0 = []
intToList x =   intToList(x `quot`  10) ++ [x `mod` 10]

getEvenOnes:: [Int] -> [Int]
getEvenOnes [] = [] -- 
getEvenOnes (x:[]) = [] 
getEvenOnes x =  last(init x) : getEvenOnes(init(init x))

getOddOnes:: [Int] -> [Int]
getOddOnes [] = [] -- 
getOddOnes (x:[]) = [] 
getOddOnes x =  last(x) : getEvenOnes(init(x))

	
sumIfMoreThan10 :: [Int] -> [Int]
sumIfMoreThan10 [] = []
sumIfMoreThan10 (x:xs) = 
    if x > 9  
        then (x - 9) : (sumIfMoreThan10 xs)
        else x : (sumIfMoreThan10 xs)

doubleList:: [Int] -> [Int] 
doubleList xs = [x*2 | x <-xs]                   

luhn:: Int -> Bool
luhn x = 
      let cardNumberList = intToList x
          oddOnes = getOddOnes cardNumberList
          evenOnes = getEvenOnes cardNumberList
          doubleEvens = doubleList evenOnes
          summEvens = sumIfMoreThan10 doubleEvens
          product = sum(summEvens) + sum(oddOnes)
      in  mod product 10 == 0


