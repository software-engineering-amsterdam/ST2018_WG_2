import Data.List
import Numeric
import Data.Char
import Test.QuickCheck 

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

-- Task 1

-- regularSum :: Int -> Int
-- regularSum n = sum [1..n]

-- regularSum':: Int -> Int
-- regularSum' n = quot (n*(n+1)) 2

sumSquare :: Int -> Int
sumSquare n = 
    let list = [1..n]
        squaredList = [x^2 | x <- list] 
    in sum squaredList

sumSquare2:: Int -> Int
sumSquare2 n = sum $ map (^2) [1..n]

sumSquare':: Int -> Int
sumSquare' n = quot (n*(n+1)*(2*n+1)) 6

-- QuickCheck 
sumSquareTest:: Int -> Bool
sumSquareTest n = sumSquare (abs n) == sumSquare' (abs n) 

-- Second in workshop

sumTriple :: Int -> Int
sumTriple n = 
    let list = [1..n]
        squaredList = [x^3 | x <- list] 
    in sum squaredList

sumTriple2:: Int -> Int
sumTriple2 n = sum (map (^3) [1..n])

sumTriple' :: Int -> Int
sumTriple' n = (quot (n * (n+1)) 2)^2

-- QuickCheck
sumTripleTest:: Int -> Bool
sumTripleTest n = sumTriple (abs n) == sumTriple' (abs n)

-- Task 2 
-- Source of questions: final.hs
--Answer: In Haskell we can only check lists up to a finite size limited by 
--memory / stack etc, while formal mathematical proofs can be used to prove a 
--statement for the entire domain (including infinite domains). 

powerSetLenght:: Int -> Int
powerSetLenght n = length (subsequences [1..n])

powerSetLenght':: Int -> Int
powerSetLenght' n = 2^n

-- QuickCheck 
powerSetLenghtTest:: Int -> Bool
powerSetLenghtTest n = powerSetLenght (abs n) == powerSetLenght' (abs n)

-- Source: Team 5 (looking for other ways)
quickCheckSets = quickCheck (\ (Positive x) -> powerSetLenghtTest x)

-- Task 3
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

permLength:: [a] -> Int
permLength n = length (perms n)


factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

-- Number of permutations in a lest are equal to the factorial of length of list
-- Source: https://www.quora.com/How-many-permutations-are-there-on-the-set-A-1-2-3-4-5
permsTest:: [Int] -> Bool
permsTest x = permLength x == factorial (length x)

-- Task 4
prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Int]
primes = 2 : filter prime [3,5..] 

-- My implementation 
takeTheReversable:: [Int]
takeTheReversable = [x | x <- filter (prime) [1..1000], prime (reversal x)]

-- Testing could be done by seeing if every element in the new list is prime and the reversable is also prime

-- Task 5

consecPrimes :: Int -> Int
consecPrimes n = sum (consecPrimes' n primes) -- sum the first consecutive numbers found  
        
consecPrimes' :: Int -> [Int] -> [Int]
consecPrimes' x primeList = 
          if prime(sum(take x primeList)) -- checks if the sum of x elements is prime
          then (take x primeList) -- returns the consecutive numbers, which sum is prime 
          else consecPrimes' x (tail (primeList)) -- recursion with the rest of the numbers of primes

-- Task 6

productSumPrimesPlus1':: Int -> Int
productSumPrimesPlus1' x = 
                let primeSumPlus1 = (product (take x primes)) + 1
                in if not(prime primeSumPlus1)
                   then primeSumPlus1 -- smallest counterexample
                else productSumPrimesPlus1' (x + 1)


-- Task 7

-- Helpers functions
intToList:: Integer -> [Integer]
intToList 0 = []
intToList x =   intToList(x `quot`  10) ++ [x `mod` 10]

getEvenOnes:: [Integer] -> [Integer]
getEvenOnes [] = [] -- 
getEvenOnes (x:[]) = [] 
getEvenOnes x =  last(init x) : getEvenOnes(init(init x))

getOddOnes:: [Integer] -> [Integer]
getOddOnes [] = [] -- 
getOddOnes (x:[]) = [] 
getOddOnes x =  last(x) : getEvenOnes(init(x))

    
sumIfMoreThan10 :: [Integer] -> [Integer]
sumIfMoreThan10 [] = []
sumIfMoreThan10 (x:xs) = 
    if x > 9  
        then (x - 9) : (sumIfMoreThan10 xs)
        else x : (sumIfMoreThan10 xs)

doubleList:: [Integer] -> [Integer] 
doubleList xs = [x*2 | x <-xs]   

listToInt:: [Integer] -> Integer
listToInt [] = 0
listToInt  x  = 
              let currEl = head x * (10^(length x - 1))
              in listToInt(tail(x)) + currEl     

-- End of helpers functions     

luhn:: Integer -> Bool
luhn x = 
      let cardNumberList = intToList x
          oddOnes = getOddOnes cardNumberList
          evenOnes = getEvenOnes cardNumberList
          doubleEvens = doubleList evenOnes
          summEvens = sumIfMoreThan10 doubleEvens
          product = sum(summEvens) + sum(oddOnes)
      in  mod product 10 == 0


isVisa:: Integer-> Bool
isVisa x =
       let cardNumberList = intToList x
       in luhn(x) && (head cardNumberList) == 4 && (length cardNumberList == 13 || length cardNumberList == 16 || length cardNumberList == 19) 

isMasterCard:: Integer -> Bool
isMasterCard x =
       let cardNumberList = intToList x  
           firsTwoNumbers = head cardNumberList * 10 + head(tail(cardNumberList))
           firtsSixNumbers = listToInt(take 6 cardNumberList)
       in luhn(x) && (length cardNumberList == 16) && (elem firsTwoNumbers [51..55] || (firtsSixNumbers >= 222100 || firtsSixNumbers <= 272099))

isAmercanExpress:: Integer-> Bool
isAmercanExpress x =
       let cardNumberList = intToList x
           firsTwoNumbers = head cardNumberList * 10 + head(tail(cardNumberList))
        in luhn(x) && (length cardNumberList == 15) && (firsTwoNumbers == 34 || firsTwoNumbers == 37)

-- Tests: 
americanExpress = [372648371028404, 341807102565054, 370251523435155, 345987022425484, 340909047606182, 347928634762516, 348680752823674, 343442065244113, 347042079136929, 346385143695524, 340946204927381, 347548711335786, 370613399889585, 342887487645084, 372398919425379, 341876831180881, 348795677441277, 372362322411624, 372421842554733, 373123817221780]
masterCard = [5280978717864703, 5204599125474746, 5129573812363225, 5180759750655013, 5323720957741616, 5416469444482453, 5494477209501354, 5110577708580207, 5222852314331609, 5344553122595566, 5135532453801549, 5149538474198439, 5171530686394691, 5418356521504390, 5162594213238244, 5214594527934890, 5561872486674726, 5122123259710228, 5492738757867709, 5201720469363898]
visaList = [4588662652014530, 4424686399080585, 4098866378874993, 4484952991952135, 4594043634676997, 4484176226428600, 4039256278975250, 4898895592347678, 4907857036516960, 4784273240097011, 4081851093264166, 4893205521264496, 4801652198982358, 4108447907599910, 4205228420961676, 4738379302593537, 4517881892937260, 4953501130847763, 4946856528152832, 4039646856261802]
-- Source: team 8
--- Possible test - One card should be only from 1 issuer 
-- Get one of the credit card checker function and list of credit card functions
testCards :: (Integer -> Bool) -> [Integer] -> Bool
testCards func [] = True
testCards func (x:xs) = (func x) && testCards func xs

--Task 8 
-- Source team: 11 - Trying to understand their logic
-- Further Explanation - Go back when studying for the exam
xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

accuses :: Boy -> Boy -> Bool
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew _ = True
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter _ = False
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)
accuses Arnold x = xor (accuses Matthew x) (accuses Peter x) 
accuses Carl x = not (accuses Arnold x)


accusers :: Boy -> [Boy]
accusers x = [y| y <- boys, accuses y x]

-- 3 persons are honest so length must be 3
guilty, honest :: [Boy]
guilty = [x| x <- boys, length (accusers x) == 3]
honest = [y| x <- guilty, y <- (accusers x)]