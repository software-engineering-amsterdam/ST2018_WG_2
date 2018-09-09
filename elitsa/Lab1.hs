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





-- if sum take 3 prime -> return the numbers
-- else sum take

-- intToList:: Int -> [Int]
-- intToList 0 = []
-- intToList x = intToList(quot x 10) ++ [mod x 10] 

-- odds:: [Int] -> [Int]
-- odds xs = [x | x <- xs, odd x]

-- specialDouble:: Int -> Int
-- specialDouble x = x
-- sumIfMoreThan10:: [Int] -> [Int]
-- sumIfMoreThan10 xs = 
		


--luhn :: Int -> Bool



-- consPrimes :: Int -> [Int]
-- consPrimes n = consPrimes' n primes

-- consPrimes' :: Int -> [Int] -> [Int]
-- consPrimes' n primeList = 
--     if prime (sum (take n primeList))
--         then sum (take n primeList) : consPrimes' n (tail primeList)
--         else consPrimes' n (tail primeList)

-- bmiTell:: a -> a -> String
-- bmiTell weight height 
-- 		| weight /height <= 18.5 = "You are"
-- 		| otherwise              =

