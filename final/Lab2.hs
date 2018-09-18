module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
--import ShowFunctions
--import Text.Show.Functions
--import Test.QuickCheck.Function -- https://stackoverflow.com/questions/5208621/show-ing-functions-used-in-quickcheck-properties

import System.IO.Unsafe



infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)



forall :: [a] -> (a -> Bool) -> Bool
forall = flip all



-- =================================
-- == Probability is uniform distribution check
-- == 2:30:00
-- =================================



--probsTest' :: Fractional a => [Float] -> a
probsTest' randomFloats = 
    let rangeCounts = countRanges randomFloats (0,0,0,0)
        validity = chiSquared rangeCounts (length randomFloats)
    in validity


--chiSquared :: (Fractional a) => (Int,Int,Int,Int) -> Int -> a
chiSquared (a,b,c,d) n = 
    let e = (fromIntegral n) / 4.0
        values = (a:b:c:d:[])
        chiSquaredValues = map (\x -> ((x - e)^2)/e) values
    in sum chiSquaredValues

--countRanges :: (Ord a1, Fractional a1, Num a) => [a1] -> (a, a, a, a) -> (a, a, a, a)
--countRanges :: [Float] -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
countRanges [] (a,b,c,d) = (a,b,c,d)
countRanges (x:xs) (a,b,c,d)
    | x < 0.25 = countRanges xs (a+1,b,c,d)
    | x < 0.5  = countRanges xs (a,b+1,c,d)
    | x < 0.75 = countRanges xs (a,b,c+1,d)
    | x < 1.0  = countRanges xs (a,b,c,d+1)


--https://en.wikipedia.org/wiki/Chi-squared_distribution 
--we have 3 degress of freedom (4 bins), and we want p-value>0.05
--verifyDistributionIsEven :: (Float a) => a -> Bool
verifyDistributionIsEven value = value < 7.81


{-
probabilityDistributionTest :: IO ()
probabilityDistributionTest = do
    print "Testing for even distribution in random number generator"
    randomFloats <- probs 10000
    print $ "Chi-squared value: "
    print (probsTest' randomFloats)
    print $ " is within acceptable range?"
    print $ verifyDistributionIsEven (probsTest' randomFloats)
-}

{-
*Lab2> probabilityDistributionTest 
"Testing for even distribution in random number generator"
"Chi-squared value: "
0.23839999999999997
" is within acceptable range?"
True
-}


-- =================================
-- == Triangle classification 
-- == 3:00:00
-- =================================

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangles = [Equilateral, Isosceles, Rectangular, Other]

--categorizes a 'triangle' based on the measures of 3 given sides  
triangle :: Int -> Int -> Int -> Shape
triangle x y z = let [a,b,c] = sort [x,y,z] in triangleHelper a b c

--True if 3 numbers satisfy the triangle inequality property, False otherwise
triangleInequalityProperty :: Int -> Int -> Int -> Bool
triangleInequalityProperty a b c = abs (a-b) < c && c < a + b && a > 0 && b > 0 && c > 0 
--https://en.wikipedia.org/wiki/Triangle_inequality

--helper function to categorize a triangle
triangleHelper :: Int -> Int -> Int -> Shape
triangleHelper x y z 
    | not $ triangleInequalityProperty x y z = NoTriangle
    | (x == y && x == z) = Equilateral
    | (x^2 + y^2 == z^2) = Rectangular
    | (x == y || x == z || y == z) = Isosceles
    | otherwise = Other
    

-- helper function to get random integers in the domain [low, high]
randInt' :: Int -> Int -> IO Int
randInt' low high = do
    value <- getStdRandom (randomR (low, high))
    if True then return value else return 0

randInt :: Int -> Int -> Int
randInt a b = unsafePerformIO (randInt' a b)


-- perform five different tests with custom randint generators and compare
-- them all to find whether our triangle function is valid.

noTriangleTest :: Bool
noTriangleTest = 
    let testList = map (\_ -> (randInt 1 5, randInt 1 5, randInt 10 20)) [1..20]
    in length (filter (/= NoTriangle) (map (\(x,y,z) -> triangle x y z) testList)) == 0
equilateralTest :: Bool
equilateralTest = 
    let testList = map (\_ -> (let x = randInt 1 30 in (x,x,x))) [1..20]
    in length (filter (/= Equilateral) (map (\(x,y,z) -> triangle x y z) testList)) == 0
isocelesTest :: Bool
isocelesTest = 
    let testList = map (\_ -> (let x = randInt 20 30 in (x,x,randInt 10 15))) [1..20]
    in length (filter (/= Isosceles) (map (\(x,y,z) -> triangle x y z) testList)) == 0
rectangularTest :: Bool
rectangularTest = 
    let testList = map (\_ -> (let x = randInt 1 30 in (3*x,4*x,5*x))) [1..20]
    in length (filter (/= Rectangular) (map (\(x,y,z) -> triangle x y z) testList)) == 0
otherTest :: Bool
otherTest = 
    let testList = map (\_ -> (randInt 110 120, randInt 121 130, randInt 200 220)) [1..20]
    in length (filter (/= Other) (map (\(x,y,z) -> triangle x y z) testList)) == 0


--static test cases for triangle checking
triangleTestCases :: [([Int], Shape)]
triangleTestCases = [
    ([3,4,5], Rectangular),
    ([0,1,2], NoTriangle),
    ([5,5,5],Equilateral),
    ([5,5,7],Isosceles),
    ([5,4,8],Other),
    ([-1,5,20],NoTriangle)
    ]

--verifies that all fixed test cases pass
triangleTestCaseVerifier :: [Bool]
triangleTestCaseVerifier = map (\([a,b,c],expected) -> triangle a b c == expected) triangleTestCases


triangleChecker :: IO Bool
triangleChecker = do
    print "Testing sample fixed triangle numbers"
    print $ and $ triangleTestCaseVerifier
    print "Testing all triangle classes and non triangles"
    return (noTriangleTest && equilateralTest && isocelesTest && rectangularTest && otherTest)


{-
*Lab2> triangleChecker
"Testing sample fixed triangle numbers"
True
"Testing all triangle classes and non triangles"
True
-}


-- =================================
-- == Testing Properties Strength == 3:30:00
-- =================================
prop1 :: Int -> Bool
prop1 x = x > 3 && even x
prop2 :: Int -> Bool
prop2 x = x > 3 || even x
prop3 :: Int -> Bool
prop3 x = (even x && x > 3) || even x
prop4 :: Int -> Bool
prop4 x = even x

stronger :: ([Char], (Int -> Bool)) -> ([Char], (Int -> Bool)) -> Bool
stronger (_, p1) (_, p2) =
    let r1 = map p1 [(-10)..10]
        r2 = map p2 [(-10)..10]
    in foldr (&&) True (zipWith (-->) r1 r2)

weaker :: ([Char], (Int -> Bool)) -> ([Char], (Int -> Bool)) -> Bool
weaker p1 p2 = stronger p2 p1

--sorts a list according to supplied function
sortWith :: (a -> a -> Bool) -> [a] -> [a]
sortWith _ [] = []
sortWith f (x:xs) = sortWith f [i | i <- xs, f i x] ++ [x] 
                    ++ sortWith f [i | i <- xs, not (f i x)]

--resulting sorted properties descendingly by strength
sortedProps :: [String]
sortedProps = 
    let contenders = [("prop1", prop1), ("prop2", prop2), ("prop3", prop3), ("prop4", prop4)]
        result = sortWith stronger contenders
    in map (\(x,_) -> x) result

{-
Output: ["prop1","prop4","prop3","prop2"]
-}

-- ==============================
-- == Recognizing Permutations == 2:00:00
-- ==============================


--this solution works only IF there are no duplicates, for the solution with duplicates see below

--checks if a given list is permutation of second one
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) y =
    if not $ elem x y then False else 
        isPermutation xs (filter (/= x) y)


{-
This solution below works IF there are duplicates in the input list
isPermutation (x:xs) ys = if elem x ys then isPermutation xs (deleteOneOccurence ys [] x) else False
deleteOneOccurence [] res _ = res
deleteOneOccurence (x:xs) ys f = if x == f then (ys++xs) else deleteOneOccurence xs (x:ys) f
-}


--testable properties of permutations
--all of these properties are incomparable and thus can not be sorted

--checks if both lists are of equal length
equalLengthProperty :: (Eq a) => [a] -> [a] -> Bool   
equalLengthProperty a b = length a == length b

-- checks if every element of A is in B
everyElementOfAInBProperty :: (Eq a) => [a] -> [a] -> Bool
everyElementOfAInBProperty l r = forall l (\x -> elem x r)

-- checks if every element of B is in A
everyElementOfBInAProperty :: (Eq a) => [a] -> [a] -> Bool
everyElementOfBInAProperty l r = everyElementOfAInBProperty r l

--checks if single list is unique
allElementsUniqueProperty :: Eq a => [a] -> Bool
allElementsUniqueProperty x = (length $ nub x) == (length x)

--checks if both lists are unique
bothListsAreUnique :: (Eq a1, Eq a2) => [a1] -> [a2] -> Bool
bothListsAreUnique l r = allElementsUniqueProperty l && allElementsUniqueProperty r

--combination of above properties that each permutation must satisfy
permutationProperty :: Eq a => [a] -> [a] -> Bool
permutationProperty a b = equalLengthProperty a b && everyElementOfAInBProperty a b && everyElementOfBInAProperty a b && bothListsAreUnique a b


--permutation list modifiers to preserve or remove 'is permutation' property

-- For quick check we cant use the generator for both lists to compare. We will use the generator for one list and manipulate this list to compare. Two versions swap and reverse
swapEvery2 :: [a]->[a]
swapEvery2 [] = []
swapEvery2 [x] = [x]
swapEvery2 (x:y:xs)  = y:x:(swapEvery2 xs)

--custom test cases for permutations
permutationTestCases :: [([Integer], [Integer], Bool)]
permutationTestCases = [
    ([],[], True),
    ([1],[1], True),
    ([1..4],[4,2,3,1], True),
    ([1,2],[1,2], True),
    ([1,2],[2,3], False),
    ([1],[2], False),
    ([1,2,3],[1,2,3,4], False)]

--verifies that all permutation test cases are satisfied
permutationTestCaseVerifier :: [Bool]
permutationTestCaseVerifier = map (\(l,r,expected) -> (permutationProperty l r) == expected) permutationTestCases

--test suite for permutations
permutationTests :: IO ()
permutationTests = do 
    print "Testing permutations via QC and swapping every 2 elements"
    quickCheckResult(\original -> let set = nub original in permutationProperty (set :: [Int])(swapEvery2 set) )
    print "Testing permutations via QC and reversing list"
    quickCheckResult(\original ->  let set = nub original in permutationProperty (set :: [Int])(reverse set) )
    print "Testing permutations via QC and multiplying by 2, should not be permutation"
    quickCheckResult(\original ->  let set = nub original in (length set > 1) --> permutationProperty (set :: [Int]) (map (*2) set) == False )
    print "Testing manually selected test cases"
    print $ and $ permutationTestCaseVerifier 

{-
*Lab2> permutationTests
"Testing permutations via QC and swapping every 2 elements"
+++ OK, passed 100 tests.
"Testing permutations via QC and reversing list"
+++ OK, passed 100 tests.
"Testing permutations via QC and multiplying by 2, should not be permutation"
+++ OK, passed 100 tests.
"Testing manually selected test cases"
True
-}

-- =============================================
-- == Recognizing and Generating Derangements 
-- == 2:00:00
-- =============================================

-- only consider the two lists if their sorted versions are equivalent to each
-- other, this checks if the two lists contain the same elements. If this is
-- not the case, they cannot be derangements of each other.
isDerangement :: [Int] -> [Int] -> Bool
isDerangement x y = if (sort x) == (sort y) then (and $ zipWith (/=) x y) else False

--computes all possible derangements for a list [0..n-1]
deran :: Int -> [[Int]]
deran n = let list = [0..n-1] in filter (\x -> isDerangement x list) (permutations list)

--tests that the count of all possible derangements is !n (subfactorial n)
deranCountProperty :: Int -> [[Int]] -> Bool
deranCountProperty n list = subfactorial n == length list


-- we restrict the domain of n to [1,20) so that the result remains computable
-- tests that all elements of lists are derangements of each other
isEveryElementDerangementTest :: Int -> Bool
isEveryElementDerangementTest n = apply' (n <= 1 && n > 10) (-->) (and (map (isDerangement [0..n-1]) (deran n)))

--tests if every element generated by 'deran' is a permutation of original
isEveryElementPermutationTest :: Int -> Bool
isEveryElementPermutationTest n = apply' (n <= 1 && n > 10) (-->) (and (map (isPermutation' [0..n-1]) (deran n)))

--alternative implementation of the isPermutation function for testing
isPermutation' :: Eq a => [a] -> [a] -> Bool
isPermutation' x y = elem x (permutations y)

--applies a binary function in infix notation
apply' :: a -> (a -> b -> c) -> b -> c
apply' a f b = f a b

--source: https://codegolf.stackexchange.com/a/113942
--computes the subfactorial of n
subfactorial :: Int -> Int
subfactorial 0 = 1
subfactorial n = n * subfactorial (n-1) + (-1)^n


--tests for the correct implemenentation of the isDerangement function:
derangementTests = do 
    print "Testing that derangement count is the subfactorial"
    quickCheckResult(\n -> (n >= 1) --> ( let x = n `mod` 8 in deranCountProperty x (deran x)))
    print "Testing derangements property for 1 to 20"
    quickCheck isEveryElementDerangementTest
    print "Testing that all derangements are permutations"
    quickCheck isEveryElementPermutationTest

{-
*Lab2> derangementTests 
"Testing that derangement count is the subfactorial"
+++ OK, passed 100 tests.
"Testing derangements property for 1 to 20"
+++ OK, passed 100 tests.
"Testing that all derangements are permutations"
+++ OK, passed 100 tests.
-}

-- =============
-- === ROT13 ===
-- === 2:00:00
-- =============



{-
ROT13 specification:

1. ROT13(x) =  
    1a) IF x is upper/lowercase letter of latin alphabet, move x 13 positions to the right and wrap around, preserving case
    1b) OTHERWISE x remains unchanged

2. ROT13(ROT13(x)) = x
   ROT13 is its own inverse
    
-}

--contains the uppercase and lowercase latin alphabet
rot13AlphabetUppercase, rot13AlphabetLowercase :: [Char]
rot13AlphabetUppercase = ['A'..'Z']
rot13AlphabetLowercase = ['a'..'z']

-- encodes single character using the ROT13 algorithm
rot13SingleChar :: Char -> Char
rot13SingleChar x 
    | x `elem` rot13AlphabetUppercase = rot13Transform (findIndex (== x) rot13AlphabetUppercase) x rot13AlphabetUppercase
    | x `elem` rot13AlphabetLowercase = rot13Transform (findIndex (== x) rot13AlphabetLowercase) x rot13AlphabetLowercase
    | otherwise = x


--helper function to transform a character to rot13
rot13Transform :: Maybe Int -> p -> [p] -> p
rot13Transform (Just index) x alphabet = alphabet !! ((index + 13) `mod` (length alphabet))
rot13Transform Nothing x _ = x 


--encodes a string using the rot13 algorithm
rot13 :: String -> String
rot13 input = map rot13SingleChar input

--property checks if a given string satisfies the rot13 identity property
rot13InverseProperty :: String -> Bool
rot13InverseProperty code = (rot13 $ rot13 code) == code

--contains both lower and upper case latin alphabet letters
rot13EncodableAlphabet :: String
rot13EncodableAlphabet = rot13AlphabetLowercase ++ rot13AlphabetUppercase 


--checks if every non latin alphabet character in original string is converted via rot13
rot13EncodedProperty :: String -> Bool
rot13EncodedProperty code = and $ map rot13EncodedPropertyChar code

--checks if every non latin alphabet character in original string remains the same
rot13NotEncodedProperty :: String -> Bool
rot13NotEncodedProperty code = and $ map rot13NotEncodedPropertyChar code

--checks if latin alphabet character will be encoded via rot13
rot13EncodedPropertyChar :: Char -> Bool
rot13EncodedPropertyChar x = (x `elem` rot13EncodableAlphabet) --> ((head $ rot13 [x]) /= x)
--checks if non latin alphabet character will remain the same after rot13
rot13NotEncodedPropertyChar :: Char -> Bool
rot13NotEncodedPropertyChar x = (not $ x `elem` rot13EncodableAlphabet) --> ((head $ rot13 [x]) == x)

rot13Tests :: IO Result
rot13Tests = do 
    print "Testing rot13 inverse property"
    quickCheckResult(\x -> rot13InverseProperty x)
    print "Testing rot13 latin alphabet character will be changed"
    quickCheckResult(\x -> rot13EncodedProperty x)
    print "Testing rot13 non-latin alphabet character is unchanged"
    quickCheckResult(\x -> rot13NotEncodedProperty x)

{-
*Lab2> rot13Tests 
"Testing rot13 inverse property"
+++ OK, passed 100 tests.
"Testing rot13 latin alphabet character will be changed"
+++ OK, passed 100 tests.
"Testing rot13 non-latin alphabet character is unchanged"
+++ OK, passed 100 tests.
Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}
-}



-- =============
-- === IBAN ===
-- === 6:00:00
-- =============

-- helper functions intToList and listToInt. these functions are each other's inversions. 
intToList :: Integer -> [Integer]
intToList 0 = []
intToList n = (intToList (quot n 10)) ++ [mod n 10]

listToInt :: [Integer] -> Integer
listToInt [] = 0
listToInt l = (last l) + 10 * listToInt (init l)


--function returns True for valid IBANs, False otherwise
iban :: String -> Bool
iban code = ibanValidLength code && ibanValidCountry code && ibanValidChecksumFormat code &&  ibanValidateModulo code


--validates that IBAN is within the allowed range
--this function does not check all country specific lengths
--we only take the minimum and maximum allowed values
ibanValidLength :: String -> Bool
ibanValidLength code = length code >= 15 && length code <= 34

--validates that the country code is in first two positions and is a letter
ibanValidCountry :: String -> Bool
ibanValidCountry code = forall (take 2 code) (\x -> isUpper x && isLetter x) 

--checks for the presence of a 2 digit checksum immediately following the country code
ibanValidChecksumFormat :: String -> Bool
ibanValidChecksumFormat code = forall (take 2 $ drop 2 code) isDigit 

--determines IBAN offset for A-Z
numberOffset :: Integer
numberOffset = - (toInteger $ ord 'A') + 10


-- checks for valid IBAN according to ISO 7064 modulo check and IBAN format rules
-- It first moves the first four digits to the end, then prceeds with the calculation.
-- If the result of the calculation has mod 97 of zero, our IBAN is valid. 
ibanValidateModulo :: String -> Bool
ibanValidateModulo iban = 
    let (firstFour, leftover) = splitAt 4 iban
        swappedFour = leftover ++ firstFour
        intForm = stringToInt swappedFour
    in (mod intForm 97) == 1

-- we start the integer conversion by inserting an accumulator
stringToInt :: String -> Integer
stringToInt string = stringToInt' string 0

-- for each character in the string, add the value of the head to the accumulator.
-- because we want to simulate appending to get the value, we multiply by ten before
-- adding digits and by 100 when adding letter values.
stringToInt' :: String -> Integer -> Integer
stringToInt' [] n = n
stringToInt' (x:xs) n = 
    let value = charToValue x
    in if value < 10 then stringToInt' xs (n*10 + value)
        else stringToInt' xs (n*100 + value)

-- the value of a chracter is its own value if the value isn't a letter but a digit
-- otherwise, do some conversion as indicated by the rules. Subtract the value of
-- 'A' to get at the baseline, and add ten to get to our actual value
charToValue :: Char -> Integer
charToValue x = if not (isLetter x) then read [x] 
    else (toInteger (ord x)) - (toInteger (ord 'A')) + 10

--list of valid IBANs generated at https://www.mobilefish.com/services/random_iban_generator/random_iban_generator.php
validIbans :: [String]
validIbans = ["GB82WEST12345698765432", "AD4191688595KBCTGKHRGYZM", "AD4699137644TDVJT9TNAC33", "AE696216259205408241845", "AL92647917519CIBTHD05U2H8H40", "AL8188806789C75VBDSYFE7JT8RQ", "AT187528538777215120", "AT631500287292735316", "AZ97PSVE6C9IQE5M0JWC8CBRJ5U3", "AZ55DZQEJKL3NZJIB84I4R4AZ2JC", "BA473050619611526256", "BA847034185550629040", "BE02611834831129", "BE38993183209905", "BG78JCEO781881BML7K2AM", "BG29TKCS078680YY8507BU", "BH52MITCVLZI7EKRRMAZZ8", "BH30TPFO1RNKM195AVPRK7", "BR0554129225980945618101435AD", "BR9325178333383395936755257HZ", "BY21BTNJ74378VIB5B1J5YJBUOG7", "BY98LV728466VQ7D9HG4SCIELNRG", "CH09531981M5F3M84OOA1", "CH3747732IYLZWBIUO7F9", "CR41653976363667206472", "CR61138643870780281035", "CY77181666732W0OK9MO71FP9QFT", "CY4043032144P6VMBCP42EWZZ0GT", "CZ0500701011515222697774", "CZ8485617709412783317386", "DE75323734430247348433", "DE18474192108609592292", "DK8658772182867299", "DK9882231742486007", "DO1342GO71371595187710081568", "DO983FDU20831039568036269792", "EE474480253094078489", "EE847394446572702817", "ES4242461128115519140579", "ES3585913197271535865795", "FI6620464595369114", "FI6656044804506513", "FO2353310063109027", "FO8865860572114390", "FR484635986406YRPB7C37CNQ77", "FR673935332609I6UU9CEY7H298", "GB77QVAA92020908648058", "GB42JSIN88833697784064", "GE21PG3151300222737824", "GE31HW7147961963101845", "GI60EUJGGZ90G5502AYCYBK", "GI62KYZX248197MKNBEG2EQ", "GL3708246420445584", "GL5923657433145030", "GR783797678UQW0XVA4M906IH2G", "GR623988469QO48OEHHLHGJ795T", "GT940IG0SQVBGKLXYNQ0755MZCCL", "GT457IKHTELPTOETQW4QA5DJCB0Z", "HR5696097956129883363", "HR7900029000289927680", "HU35561014514577636317765985", "HU07675568826301397810796833", "IE82MBQF95636985890031", "IE70YAQX91756188770636", "IL061704177385884933485", "IL759658792375469613502", "IQ66OYLR002585034811165", "IQ33AQTG201448577439088", "IS534320914789017766765242", "IS744953893877888517703327", "IT41A2464674442N9UJ58MRV52F", "IT54Q8323578135V15YFILQLCQ2", "JO07HCZN1390P3CFRWO5PFKOZEFYAT", "JO04ROOB747642OUY4VVTEA8E8MF60", "KW87GMKE8596DGCY4BXEEN32TP78AS", "KW20WBZU89RIUV2KF2H295FUZQM0BZ", "KZ728674JK6WEF791XKT", "KZ474218JHQDHFKO8QZ5", "LB5587918EMU483GWSUK5HNDQ3VJ", "LB985311F4XQ0DW6JUCMG71WUC3I", "LC72XICCQJ4L4TXOYHTG7XOKW6R6IUV8", "LC53HMLCAZ05WQ8UBFBARW6AIUL89V2I", "LI57633635O1UBTORXN1W", "LI2478016OPB828VA396N", "LT367811574986624132", "LT107638589265707594", "LU55779IFGB9W9MH1N68", "LU15124SOR3J03LBVPLT", "LV63TTCXKKS6CXOR8K5ZH", "LV27SIATN7ZLFICZ6FIJ0", "MC272584403191YW3PUQ7WKEK64", "MC584147481282OU9HZ3JCLZS70", "MD88OJICR8Z560CV5Z56RLR9", "MD34YDC8NCGPGDHP1FJCLO6P", "ME04836526530797062693", "ME38193344146057837275", "MK27300SMNOTH61BR15", "MK98076F8MQNKSKMK35", "MR7581073934037132495036327", "MR9198356711796580929494537", "MT05OAKX88202ZUV686SRTDLFO3PMOX", "MT86KRMO134227SEERIMR8V7IPN9MMZ", "MU88TMNR0567672225354069719QFY", "MU02BSVW5255797069306607843JQA", "NL15SNSD1856129322", "NL09UYBL7722185460", "NO2342034237064", "NO2168330172255", "PK86JQJC912IQU6L9ZAUNHBE", "PK47QHFQID9HEV74AVAAZVYP", "PL79804386633808801328774446", "PL02479493361858307984715078", "PS87TERJ244J2R389F78COVFK2YYR", "PS72TULCEHPYY6L2X4BJVYZW2GZL9", "PT54925048712726506012121", "PT52731447700575942975818", "QA19CPNM7M6GSJ8RHK193VISJEVV1", "QA96YJPZSRVYGZDBG467C3SNR7LOC", "RO04OYZPMALS0603FQ66DDVN", "RO96VPAB2BEU4WJSMKA2YBS9", "RS85990804997303599384", "RS79691452161895520088", "SA1612FBV017RAOW0FFY6MI4", "SA7852BX0TF16FCT06WXM6QK", "SC21SKQT48397576814218878792BLT", "SC49BHPC24249659100274263645RUC", "SE1427739638040573160054", "SE3555829824039845023616", "SI85263838359592515", "SI94298882643105654", "SK4966726045742512919467", "SK6417545335470924950926", "SM55M9144704930ITBSM6VT6ODJ", "SM74I4990804171TV1CFZN664IA", "ST70025736394519337487434", "ST57928407446221033032677", "SV31BPAF67429031273059240765", "SV37BACK56369176007791803450", "TL309311665903143076228", "TL506496396787421957197", "TN9451047133855297489170", "TN8341827991659205574472", "TR4660960LJBQ0A3BTOJF8GBDH", "TR110858750WLRCLO6F5YL6B8A", "UA95465366MKXGG7M3JB2IAP12IGO", "UA76776931P09ZAM38BKZFDJ7FISO", "VG19HKUD2090704270585150", "VG09IFIZ4245377217696104", "XK522078769488135293", "XK164946249920719752"]


--list of invalid IBANs
invalidIbansStatic :: [String]
invalidIbansStatic = [
    "",  --invalid length
    "ZZZZ123444444444444444444444", --missing checksum
    "AL47212110090000000235698742", --changed last digit 1-> 2
    "AL4721211009000000023569874", --dropped last digit
    "AL47212110090010000235698741" --changed 0->1 in the middle
    ] 


{-
Source: https://en.wikipedia.org/wiki/International_Bank_Account_Number
The check digits enable the sending bank (or its customer) to perform a sanity check 
of the routing destination and account number from a single string of data 
at the time of data entry.[4] This check is guaranteed to detect 
any instances where a single character has been omitted, 
duplicated, mistyped or where two characters have been transposed. 
Thus routing and account number errors are virtually eliminated.[9]
-}

--functions to transform (valid) IBAN to an invalid state for testing

--transpose single letter to another via rot13
ibanTestTranspose :: String -> Int -> Bool
ibanTestTranspose code pos = let index = (pos `mod` (length code)) in let newLetter = (rot13SingleChar (code !! index)) in (isLetter (code !! index)) --> (not $ iban ((changeNthElement index (\_-> newLetter) code )))



--increments first found number in IBAN, this should fail the ISO 7064 modulo check, 
--as any more transformations are not guaranteed to be caught
ibanIncrementSingleNumber :: String -> Int -> Int -> String
ibanIncrementSingleNumber "" _ _ = ""
ibanIncrementSingleNumber (x:xs) delta base = if isDigit x then (show $ mod (digitToInt x + delta) base) ++ xs  else x : ibanIncrementSingleNumber xs delta base

ibanTestIncrementSingleNumber :: String -> Int -> Bool
ibanTestIncrementSingleNumber code delta = delta /= 0 -->  (iban $ ibanIncrementSingleNumber code delta 10)


--test suite for IBAN
ibanTestCases :: IO Result
ibanTestCases = do 
    print "Testing valid IBANs"
    print $ and $ map iban validIbans
    print "Testing invalid IBANs"
    print $ and $ map not (map iban invalidIbansStatic)
    print "Testing converted IBANs to *probably* invalid IBANs via QuickCheck number increment"
    quickCheckResult(\delta -> (delta /= 0 && mod delta 10 /= 0) --> (and $ map not (map (\code -> ibanTestIncrementSingleNumber code delta) validIbans)))
    print "Testing converted IBANs to *probably* invalid IBANs via QuickCheck single letter transposition"
    quickCheckResult(\p1 -> (and $ map (\code -> ibanTestTranspose code p1) validIbans))

{-
*Lab2> ibanTestCases 
"Testing valid IBANs"
True
"Testing invalid IBANs"
True
"Testing converted IBANs to *probably* invalid IBANs via QuickCheck number increment"
+++ OK, passed 100 tests.
"Testing converted IBANs to *probably* invalid IBANs via QuickCheck single letter transposition"
+++ OK, passed 100 tests.
Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}
-}




--source: https://stackoverflow.com/a/30557189 
swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt f s xs = map snd . foldr (\x a -> 
        if fst x == f then ys !! s : a
        else if fst x == s then ys !! f : a
        else x : a) [] $ ys
    where ys = zip [0..] xs                        

----------------------------------------

--source: https://stackoverflow.com/a/15530742
changeNthElement :: Int -> (a -> a) -> [a] -> [a]
changeNthElement idx transform list
    | idx < 0   = list
    | otherwise = case splitAt idx list of
                    (front, element:back) -> front ++ transform element : back
                    _ -> list    -- if the list doesn't have an element at index idx

----------------------

--source: https://stackoverflow.com/a/31978353
deleteAt idx xs = lft ++ rgt  where (lft, (_:rgt)) = splitAt idx xs
-------------


-- ===============
-- === EULER 1 ===
-- ===============
-- apply the filter to the search space, outcome is 233168
sumOf35Dividers :: Int
sumOf35Dividers = sum (filter divBy3or5 [1..999])

divBy3or5 :: Int -> Bool
divBy3or5 x = mod x 3 == 0 || mod x 5 == 0