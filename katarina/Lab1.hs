
module Lab1 where
import Data.List
import Test.QuickCheck    

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

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

-------------------------------------------------------------------------------------------
--Katarina Lang--
-------------------------------------------------------------------------------------------

-- Excercise 1

    -- Problem 2 

sumPot2 n = sum (map (\x -> x^2) [1..n])
sumPot2' n = quot (n*(n+1)*(2*n+1)) 6

checkSumPot2 n = sumPot2 (abs n) == sumPot2' (abs n)
    -- abs because the statements only holds for natural numbers
 
   --Excercise 3

sumPot3 n = sum (map (\x -> x^3) [1..n])
sumPot3' n = (quot (n*(n+1)) 2)^2

checkSumPot3 n = sumPot3 (abs n) == sumPot3' (abs n)



-- Excercise 2

sizeOfPowerSet n = length (subsequences [1..n])
sizeOfPowerSet' n = 2^n

formularForListsHolds n = sizeOfPowerSet(abs n) == sizeOfPowerSet' (abs n)

-- This formular is hard to test, because it should hold for every number that is greater than 0. It is not possible to check this statement for an infinite number of numbers in a finite time.
-- Quick Check only puts some numbers and checks if it is right, not a proof.