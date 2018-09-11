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
-- === 1 === 30 min
-- =========

-- quadSum definition, using quot because the / operator
-- makes Haskell whine about Fractionals
quadSum :: Int -> Int
quadSum n = quot (n*(n+1)*(2*n+1)) 6

quadSum' :: Int -> Int
quadSum' n = sum (map (^2) [1..n])

-- tripSum definition, also using quot instead of /
tripSum :: Int -> Int
tripSum n = (quot (n * (n+1)) 2)^2

tripSum' :: Int -> Int
tripSum' n = sum (map (^3) [1..n])

-- quickCheck test definitions, abs function is there
-- to make sure we don't get any negative tests that will
-- fail just because the sums have input domain [0,->)
quadSumTest x = quadSum (abs x) == quadSum' (abs x)
tripSumTest y = tripSum (abs y) == tripSum' (abs y)

-- to test, call:
-- quickCheck quadSumTest
-- quickCheck tripSumTest

-- =========
-- === 2 === 20 min
-- =========

{- 
Answer: In Haskell we can only check lists up to a finite size limited by 
memory / stack etc, while formal mathematical proofs can be used to prove a 
statement for the entire domain (including infinite domains). 

The only thing we are testing is whether our implementation and subsequences
work together sufficiently to give us results that correspond to the outcome
of the 2^n function. If subsequences had some separate implementation for lists
of length > 21, the test would fail and we would never know. 
-}
powerLength :: Int -> Int
powerLength n = length (subsequences [1..n])

powerLength' :: Int-> Int
powerLength' n = 2^n

-- helperfunction to reduce the test numbers to a handleable order of mgnitude
lessThan20 :: Int -> Int
lessThan20 n = (abs n) `mod` 20

-- quickCheck test definition
powerLengthTest n' = 
    let n = lessThan20 n'
    in  powerLength n == powerLength' n

-- =========
-- === 3 === 1.5 h
-- =========
-- helper function that counts all of the instances of a given
-- Int inside a given list
count :: Int -> [Int] -> Int
count x = length . filter (==x)

-- helper function, recursively computes the factorial of an Int
fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n-1)

-- The idea here is to use this formula: The amount of permutations of
-- a list is the factorial of the length of that list divided by the factorials
-- of the numbers of instances of each item in the list, all multiplied
-- together. Explanation and proof can be found at:
--     https://math.stackexchange.com/questions/119044/what-is-the-proof-of-permutations-of-similar-objects

-- The amount of possible permutations equals the factorial of the length of the list
-- over the instanceCounts factorialed together, calculated by instanceFact. The
-- division of these two values gives us our answer.
permCalc :: [Int] -> Int
permCalc l = quot (fact (length l)) (instanceFact l)

-- The instanceFact function takes a list of ints and converts them into a list
-- of Ints denoting how many times each of these ints occurred in the original
-- list. The result is then the product of all of the factorials of this list.
-- the list of instanceCounts is returned by the instanceCount function, this
-- helper function only applies a factorial function to each list element
-- and multiplies them all together using the fold function.
instanceFact :: [Int] -> Int
instanceFact l = foldl (*) 1 (map (\x -> fact(x)) (instanceCount l))

-- instanceCount converts a ist of integers into a list denoting how many times
-- each item occurred in that list. for instance, the list [1,1,1,1,2,3,3,5,5,5]
-- will be converted into [4,1,2,3] (four ones, one two, two threes, and three
-- fives). It does this by starting a recursive chain with an empty list as 
-- starting argument.
instanceCount :: [Int] -> [Int]
instanceCount l = instanceCount' [] l

-- instanceCount' starts out by taking the current counts and instances lists,
-- and stores the instance that is to be counted in this instance. It then
-- stores the count in the thisInstanceCount variable and appends it to nextCounts.
-- Finally, it filters all instances of this number from the instances list.
-- This way, the counts list accumulates all the different instanceCounts and
-- the insatnces list eventually empties. Once it is empty, the counts array
-- contains the function its return value.
instanceCount' :: [Int] -> [Int] -> [Int]
instanceCount' l [] = l
instanceCount' counts instances = 
    let thisInstance = head instances
        thisInstanceCount = count thisInstance instances
        nextCounts = thisInstanceCount:counts
        nextInstances = filter (/=thisInstance) instances
    in instanceCount' nextCounts nextInstances

-- helper permutations function. This does generate a lot of duplicates.
perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

-- various test functions, testPermCalc tests all in one go.
testPermCalc1 = 
    let testList = [1,1,1,2,3,3,3,4]
        in permCalc testList == (length $ rmDups $ perms testList)
testPermCalc2 = 
    let testList = [1,2,3,4,5,6]
        in permCalc testList == (length $ rmDups $ perms testList)
testPermCalc3 = 
    let testList = [1]
        in permCalc testList == (length $ rmDups $ perms testList)
testPermCalc = and [testPermCalc1, testPermCalc2, testPermCalc3]

-- =========
-- === 4 ===
-- =========
-- The revPrimes function filters the list of all odd numbers lower than 10000
-- by only selecting the numbers that are both prime and its reversal is also
-- prime. Not efficient, but readable and works.
revPrimes :: [Int]
revPrimes = filter (\x -> (prime x)  && (prime $ reversal x)) (2:[3,5..9999])

-- testing this function could most easily be done by implementing another 
-- function that does the same thing and see if their return values match.
-- If they do, the odds that they are both wrong in exactly the same way are a
-- lot lower than the probability that both definitions are right.

-- =========
-- === 5 ===
-- =========
consecPrimes :: Int -> Int
consecPrimes n = sum (consecPrimes' n primes) -- sum the first consecutive numbers found  

consecPrimes' :: Int -> [Int] -> [Int]
consecPrimes' x primeList = 
          if prime(sum(take x primeList)) -- checks if the sum of x elements is prime
          then (take x primeList) -- returns the consecutive numbers, which sum is prime 
          else consecPrimes' x (tail (primeList)) -- recursion with the rest of the numbers of primes

consecPrimeTest = consecPrimeTest1 && consecPrimeTest2
consecPrimeTest1 = consecPrimes 101 == 37447
consecPrimeTest2 = consecPrimes 5 == 101

-- =========
-- === 6 === 45 min
-- =========
-- start the search at taking 1 consecutive prime
conjCounter :: [Int]
conjCounter = conjCounter' 1

-- if the multiplication of the first n consecutive primes plus one => foldr (*)
-- 1 (take n primes) is not prime, we have found a counterexample. Append it to
-- the list if so. In either case keep looking for the next n by starting a new
-- iteration with n+1 as argument.
conjCounter' :: Int -> [Int]
conjCounter' n =
    let testValue = ((foldr (*) 1 (take n primes)) + 1) 
    in if not $ prime testValue
        then testValue : conjCounter' (n+1)
        else conjCounter' (n+1)

-- the solution is the smalles prime number that is equal to the product of
-- n consecutive primes +1 and is not prime itself. 
solution6 = head conjCounter -- => 30031

-- =========
-- === 7 === 1 hour
-- =========

-- helper functions intToList and listToInt. these functions are each other's 
-- inversions. 
intToList :: Int -> [Int]
intToList 0 = []
intToList n = (intToList (quot n 10)) ++ [mod n 10]

listToInt :: [Int] -> Int
listToInt [] = 0
listToInt l = 
    (last l) + 10 * listToInt (init l)

-- odds takes a list of integers and a boolean, and returns all of the odd
-- positions, counted from the back of the list if the boolean is True. If
-- False, it returns all of the even items in the list.
getEvenOnes:: [Int] -> [Int]
getEvenOnes [] = [] -- 
getEvenOnes (x:[]) = [] 
getEvenOnes x =  last(init x) : getEvenOnes(init(init x))

getOddOnes:: [Int] -> [Int]
getOddOnes [] = [] -- 
getOddOnes (x:[]) = [] 
getOddOnes x =  last(x) : getEvenOnes(init(x))

-- specialdouble doubles all the values in a list, if the doubled value s higher
-- than 9, it instead computes the sum of the doubled values digits.
specialDouble :: [Int] -> [Int]
specialDouble [] = []
specialDouble (x:xs) = 
    if x+x > 9 
        then (x+x-9) : (specialDouble xs)
        else (x+x) : (specialDouble xs)

-- The luhn function converts the given integer to a list of integers, gets the
-- odd numbered values and the even numbered ones, specialDoubles the odd values
-- and computes the sum of all the results. If this number mod 10 is equal to
-- zero, we have found a valid Luhn number.
luhn :: Int -> Bool
luhn n = 
    let nList = intToList n 
        evens = getEvenOnes nList
        odds = getOddOnes nList
        total = sum ((specialDouble evens) ++ odds)
    in mod total 10 == 0

-- The america express test converts the first two items of the list form of the
-- given int into an integer for easy comparison, we have to check if the length
-- of the list is equal to 15 and whether the first two digits match 34 or 47.
-- If all this is the case, we have a match if the luhn is also valid.
isAmericanExpress :: Int -> Bool
isAmericanExpress n = 
    let l = intToList n
        firstTwo = listToInt $ take 2 l
    in  (length l == 15) &&
        (firstTwo == 34 || firstTwo == 37) &&
        (luhn n) 

-- same as above, only this time we also take the first six digits of the int
-- to check if they are with in the range [222100,272099], or the first two
-- digits are within the range [51,55]. If this is true and the luhn function
-- returns true, we have a match.
isMaster :: Int -> Bool
isMaster n =
    let l = intToList n
        firstTwo = listToInt $ take 2 l
        firstSix = listToInt $ take 6 l
    in  (length l == 16) &&
        ((firstSix >= 222100 && firstSix <= 272099) ||
            (elem firstTwo [51..55])) &&
        (luhn n)

-- isVisa checks for a valid length, checks if the first digit is equal to 4,
-- and checks with the luhn function for vailidity. If everything checks out,
-- we have a match.
isVisa :: Int -> Bool
isVisa n = 
    (elem (length (intToList n)) [13,16,19]) &&
    ((head (intToList n)) == 4) &&
    (luhn n)

-- at the botom of this file, we defined a few lists containing randomly generated
-- credit card numbers that we use to test the functions. If the functions pass
-- on all the different examples, our functions are tested on 20 different examples.
testVisa = and (map isVisa visa)
testMX = and (map isAmericanExpress mx)
testMasterCard = and (map isMaster masterCard)
testLuhn = and (map luhn (visa ++ masterCard ++ mx))

-- =========
-- === 8 === 3 hours
-- =========

-- Our approach is to construct a search space of all possible world states.
-- Each world state is a tuple of five Integers of value 0 or 1, indicating
-- for each Boy whether they are being truthful or not. We construct this list
-- of tuples by taking all combinations of the list [1,1,1,0,0], removing the
-- duplicates, and mapping a list to tuple converter over this array. 
-- The truthOptions does all this in one fell swoop / line of code
combinations :: Eq a => [a] -> [[a]]
combinations l = rmDups (perms l)

rmDups :: Eq a => [[a]] -> [[a]]
rmDups [] = []
rmDups (x:xs) = if elem x xs
    then rmDups xs else x:(rmDups xs)

listToTuple :: Num a => [a] -> (a,a,a,a,a)
listToTuple (a:b:c:d:e:[]) = (a,b,c,d,e)

truthOptions :: (Eq a, Num a) => [(a,a,a,a,a)]
truthOptions = map listToTuple (combinations [1,1,1,0,0])

-- Having found our search space, we can start eliminating some of them by applying
-- predicates that we can derive from the logic of all of the statements. If 
-- some combinations of true and/or false statements yield internally inconsistent
-- formulas, we can remove them from the search space. This way, we are hopefully
-- left with a singleton list containing indicators of who is and who isn't
-- telling the truth.

-- The statements by Albert and Carl are mutually incompatible, so we can eliminate
-- all solutions where they are either both lying or telling the truth
predicate1 (m,p,j,a,c) = if a == c then False else True

-- If Albert is telling the truth, then Albert and Peter can't both 
-- either be telling the truth or both be lying. Hence, if they are
-- equally truthful, eliminate the hypothesis
predicate2 (m,p,j,a,c) = if a == 1 && m == p then False else True

-- If Albert is lying, the only way for his statement to be evaluated as
-- False is if Matthew and Peter ARE equally as truthful. If this is not
-- the case, we have found an inconsistency, eliminate.
predicate3 (m,p,j,a,c) = if a == 0 && m /= p then False else True

-- If Jack is telling the truth, then neither Michael nor Peter can be telling
-- telling the truth. If either one of them is, we found an inconsistent hypothesis
predicate4 (m,p,j,a,c) = if j == 1 && (m == 1 || p == 1) then False else True

-- We can find the remaining plausible Hypotheses by using the above predicates
-- as filters over our search space (the truthOptions array). If we are left
-- with a singleton list, we have done our job.
appliedFilters = filter predicate4 $ filter predicate3 $
        filter predicate2 $ filter predicate1 truthOptions

-- honest takes the head of the applied filters and translates the values
-- of each integer into a list of honest boys. 
honest :: [Boy]
honest = 
    let (m,p,j,a,c) = head appliedFilters
        x1 = if m == 1 then [Matthew] else []
        x2 = if p == 1 then [Peter] else []
        x3 = if j == 1 then [Jack] else []
        x4 = if a == 1 then [Arnold] else []
        x5 = if c == 1 then [Carl] else []
    in x1 ++ x2 ++ x3 ++ x4 ++ x5

guilty :: [Boy]
guilty = 
    -- I only care about the truthfulness of Michael and Peter because
    -- only their statements influence the outcome of this function
    let (m',p',_,_,_) = head appliedFilters
        m = m' == 1
        p = p' == 1
    in  if m && p then [Jack] 
        else if m && (not p) then [Arnold, Peter]
            else if (not m) && p then [Matthew]
                else [Carl]

-- ===============
-- === EULER 9 ===
-- ===============
-- generator for all pythagorean triplets, we assume the values of a, b, and c 
-- won't be over 500 so we don't make the search space any bigger
pythTrips :: [(Int, Int, Int)]
pythTrips = [(x,y,z) | x <- [1..500], y <- [x..500], z <- [y..500], x^2 + y^2 == z^2]

-- ask the actual function to find the special pythagorean triplet in the
-- list pythTrips, return the productt of this triplet on success
specPythTripProd :: Int
specPythTripProd = 
    let (a,b,c) = specPythTrip' pythTrips
    in a*b*c

-- check the head of the list for being the correct triplet, search the tail if
-- we need to keep looking. Otherwise, return the triplet in tuple form.
specPythTrip' :: [(Int, Int, Int)] -> (Int, Int, Int)
specPythTrip' ((a,b,c):xs) = if a + b + c == 1000
    then (a,b,c) else specPythTrip' xs

-- ================
-- === EULER 10 ===
-- ================
-- define the sieve as described in the lab slides
sieve :: [Int] -> [Int]
sieve [] = []
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)

-- Ask the actual function for the sum of all primes lower than n.
-- Pass the sieve and an accumulator with value 0 as starting params.
sumOfTwoMilPrimes :: Int -> Int
sumOfTwoMilPrimes n = sumOfTwoMilPrimes' (sieve (2:[3,5..])) 0 n

-- take the head of the primes list, the current accumulator, and the max value
-- of the primes. If the next prime is higher than max, we already have our total.
-- If not, add the current prime to the accumulator and keep looking in the tail.
sumOfTwoMilPrimes' :: [Int] -> Int -> Int-> Int
sumOfTwoMilPrimes' (x:xs) n m = 
    if x >= m then n
        else sumOfTwoMilPrimes' xs (n+x) m

-- ================
-- === EULER 49 ===
-- ================
-- Helper function to check if two ints are permutations of each other. This is
-- done by converting the Int to a list, sorting it, and converting to an Int again.
-- This way, only permutations will match
permutation :: Int -> Int -> Bool
permutation x y = listToInt (sort (intToList x)) == listToInt (sort (intToList y))

-- ask the actual function to find the prime triplet, return the appended string
-- forms of these Integers after recasting them to an Int using read.
seqPermPrimes :: Int
seqPermPrimes = 
    let (x,y,z) = seqPermPrimes'
    in read ((show x) ++ (show y) ++ (show z))

-- return the head of the tail of the list containing tuples (x,y,z), where:
-- x is an uneven number between 1001 and 9999, x is prime, y is an uneven number
-- between x+2 and 9999, y is prime, z is y + the difference between y and x,
-- z is smaller than 10000, z is prime, and x, y, and z are permutations of each
-- other. We take the head of the tail so that we don't get the example solution
-- as a return value.
seqPermPrimes' :: (Int, Int, Int)
seqPermPrimes' = head (tail [(x,y,z) |  x <- [1001,1003..9999], 
                            prime x,
                            y <- [x+2,x+4 .. 9999],
                            prime y,
                            let z = y + (y-x),
                            z < 10000, 
                            prime z,
                            permutation x y,
                            permutation y z])
-- => 296962999629

mx = [372648371028404, 341807102565054, 370251523435155, 345987022425484, 340909047606182, 347928634762516, 348680752823674, 343442065244113, 347042079136929, 346385143695524, 340946204927381, 347548711335786, 370613399889585, 342887487645084, 372398919425379, 341876831180881, 348795677441277, 372362322411624, 372421842554733, 373123817221780]
masterCard = [5280978717864703, 5204599125474746, 5129573812363225, 5180759750655013, 5323720957741616, 5416469444482453, 5494477209501354, 5110577708580207, 5222852314331609, 5344553122595566, 5135532453801549, 5149538474198439, 5171530686394691, 5418356521504390, 5162594213238244, 5214594527934890, 5561872486674726, 5122123259710228, 5492738757867709, 5201720469363898]
visa = [4588662652014530, 4424686399080585, 4098866378874993, 4484952991952135, 4594043634676997, 4484176226428600, 4039256278975250, 4898895592347678, 4907857036516960, 4784273240097011, 4081851093264166, 4893205521264496, 4801652198982358, 4108447907599910, 4205228420961676, 4738379302593537, 4517881892937260, 4953501130847763, 4946856528152832, 4039646856261802]