module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
--import ShowFunctions

--import Text.Show.Functions
import Test.QuickCheck.Function -- https://stackoverflow.com/questions/5208621/show-ing-functions-used-in-quickcheck-properties


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



-- ====== 1 ======== 3:00 h
freqDistCount :: [Float] -> (Int, Int, Int, Int)
freqDistCount list = let 
    q1 = length $ filter (\x -> x > 0 && x < 0.25) list
    q2 = length $ filter (\x -> x >= 0.25 && x < 0.50) list
    q3 = length $ filter (\x -> x >= 0.5 && x < 0.75) list
    q4 = length $ filter (\x -> x >= 0.75 && x < 1) list
    in (q1,q2,q3,q4)


--TODO: improve efficiency and method
freqDistPerc (a,b,c,d) = let sum = a+b+c+d
                        in (cDiv a sum, cDiv b sum, cDiv c sum, cDiv d sum)

cDiv a b = (fromIntegral a) / (fromIntegral b)

middlePoint, toleranceP :: Double 
middlePoint = 0.25
toleranceP = 0.01

isEvenlyDistributed :: (Double,Double,Double,Double) -> Bool
isEvenlyDistributed (a,b,c,d) =  bounded a && bounded b && bounded c && bounded d

bounded :: Double -> Bool
bounded x = inRange x (middlePoint - toleranceP) (middlePoint + toleranceP)

--inRange :: Int -> Int -> Int -> Bool
inRange value min max = value >= min && value <= max


testEvenDistributionGenerator = do
        print "Testing for even distribution in RNG"
        x <- probs 10000
        return (isEvenlyDistributed $ freqDistPerc $ freqDistCount x)


-- ========== 2 =========

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangles = [Equilateral, Isosceles, Rectangular, Other]
           
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z = let [a,b,c] = sort [x,y,z] in triangleHelper a b c

--True if 3 numbers satisfy the triangle inequality property, False otherwise
triangleInequalityProperty :: Integer -> Integer -> Integer -> Bool
triangleInequalityProperty a b c = abs (a-b) < c && c < a + b && a > 0 && b > 0 && c > 0 
--https://en.wikipedia.org/wiki/Triangle_inequality

triangleHelper :: Integer -> Integer -> Integer -> Shape
triangleHelper x y z 
    | not $ triangleInequalityProperty x y z = NoTriangle
    | (x == y && x == z) = Equilateral
    | (x^2 + y^2 == z^2) = Rectangular
    | (x == y || x == z || y == z) = Isosceles
    | otherwise = Other
    

    
triangleTestCases :: [([Integer], Shape)]
triangleTestCases = [
    ([3,4,5], Rectangular),
    ([0,1,2], NoTriangle),
    ([5,5,5],Equilateral),
    ([5,5,7],Isosceles),
    ([5,4,8],Other),
    ([-1,5,20],NoTriangle)
    ]
    

triangleTestCaseVerifier = map (\([a,b,c],expected) -> triangle a b c == expected) triangleTestCases


triangleChecker = do
--    quickCheckResult(\[a,b,c] ->  triangleInequalityProperty (a::Positive Integer) (b::Integer) (c::Integer) --> ((triangle a b c) `elem` triangles))
    print "Testing sample triangle numbers"
    print $ and $ triangleTestCaseVerifier

-- ========= 3 =======


stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 


--a) Implement all properties from the Exercise 3 from Workshop 2 as Haskell functions of type Int -> Bool. Consider a small domain like [-10..10]


property3_1a = (\ x -> even x && x > 3) 
property3_1b = even


property3_2a = (\ x -> even x || x > 3) 
property3_2b = property3_1b


property3_3a = (\ x -> (even x && x > 3) || even x) 
property3_3b = property3_1b


property3_4a = property3_1b 
property3_4b = (\ x -> (even x && x > 3) || even x)

propertyList = [property3_1a, property3_1b, property3_2a, property3_2b, property3_3a, property3_3b, property3_4a, property3_4b]


strongerCompare l r = (stronger domain l r) || ((weaker domain l r) && (stronger domain l r))


domain = [-10..10]

sortBasedOnStrongness [] = []
sortBasedOnStrongness (x:xs) = sortBasedOnStrongness matched ++ (x : sortBasedOnStrongness notMatched)
  where matched = [y | y <- xs, strongerCompare y x]
        notMatched = [y | y <- xs, not $ strongerCompare y x]


--time: 3hr no tests



isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) ys = if elem x ys then isPermutation xs (deleteOneOccurence ys [] x) else False


deleteOneOccurence [] res _ = res
deleteOneOccurence (x:xs) ys  f = if x == f then (ys++xs) else deleteOneOccurence xs (x:ys) f


--testable properties
equalLengthProperty a b = length a == length b
everyElementOfAInBProperty l r = forall l (\x -> elem x r)
everyElementOfBInAProperty l r = everyElementOfAInBProperty r l

permutationProperty a b = equalLengthProperty a b && everyElementOfAInBProperty a b && everyElementOfBInAProperty a b


--permutation list modifiers to preserve or remove 'is permutation' property

-- For quick check we cant use the generator for both lists to compare. We will use the generator for one list and manipulate this list to compare. Two versions swap and reverse
swapFirst2 :: [a]->[a]
swapFirst2 [] = []
swapFirst2 [x] = [x]
swapFirst2 (x:y:xs)  = y:x:(swapFirst2 xs)

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

permutationTests = do 
    print "Testing permutations via QC and swapping 2 elements"
    quickCheckResult(\original -> let set = nub original in permutationProperty (set :: [Int])(swapFirst2 set) )
    print "Testing permutations via QC and reversing list"
    quickCheckResult(\original ->  let set = nub original in permutationProperty (set :: [Int])(reverse set) )
    print "Testing permutations via QC and multiplying by 2, should not be permutation"
    quickCheckResult(\original ->  let set = nub original in (length set > 1) --> permutationProperty (set :: [Int]) (map (*2) set) == False )
    print "Testing manually selected test cases"
    print $ and $ permutationTestCaseVerifier 
    
-- =========== IBAN
--time: 7:45h with testing


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
ibanValidLength :: String -> Bool
ibanValidLength code = length code >= 15 && length code <= 34

--validates that the country code is in first two positions and is a letter
ibanValidCountry :: String -> Bool
ibanValidCountry code = forall (take 2 code) (\x -> isUpper x && isLetter x) 

--checks for the presence of a 2 digit checksum immediately following the country code
ibanValidChecksumFormat :: String -> Bool
ibanValidChecksumFormat code = forall (take 2 $ drop 2 code) isDigit 

--moves initial 4 letters of IBAN to the end
ibanMoveInitEnd :: String -> String
ibanMoveInitEnd code =  let (a, b) = splitAt 4 code in b ++ a

--determines IBAN offset for A-Z
numberOffset :: Integer
numberOffset = - (toInteger $ ord 'A') + 10

--converts numbers to numbers and uppercase letters to the range 10-35
ibanStringExpand :: Char -> Integer
ibanStringExpand letter = if isDigit letter then toInteger $ digitToInt letter 
                          else (toInteger $ ord letter) + numberOffset


-- converts numbers >= 10 to their separate digits
-- 1 becomes [1], 1234 becomes [1,2,3,4]
ibanSplitIntArray :: [Integer] -> [Integer]
ibanSplitIntArray [] = []
--ibanSplitIntArray (x:[]) = intToList x
ibanSplitIntArray (0:xs) = [0] ++ ibanSplitIntArray xs
ibanSplitIntArray (x:xs) = intToList x ++ ibanSplitIntArray xs

-- ISO 7064 modulo check
ibanCalcMod :: [Integer] -> Bool
ibanCalcMod xs = (listToInt $ map toInteger xs) `mod` 97 == 1

-- checks for valid IBAN according to ISO 7064 modulo check and IBAN format rules
ibanValidateModulo :: String -> Bool
ibanValidateModulo code = ibanCalcMod $ ibanSplitIntArray $ map ibanStringExpand $ ibanMoveInitEnd code


validIbans = ["GB82WEST12345698765432", "AD4191688595KBCTGKHRGYZM", "AD4699137644TDVJT9TNAC33", "AE696216259205408241845", "AL92647917519CIBTHD05U2H8H40", "AL8188806789C75VBDSYFE7JT8RQ", "AT187528538777215120", "AT631500287292735316", "AZ97PSVE6C9IQE5M0JWC8CBRJ5U3", "AZ55DZQEJKL3NZJIB84I4R4AZ2JC", "BA473050619611526256", "BA847034185550629040", "BE02611834831129", "BE38993183209905", "BG78JCEO781881BML7K2AM", "BG29TKCS078680YY8507BU", "BH52MITCVLZI7EKRRMAZZ8", "BH30TPFO1RNKM195AVPRK7", "BR0554129225980945618101435AD", "BR9325178333383395936755257HZ", "BY21BTNJ74378VIB5B1J5YJBUOG7", "BY98LV728466VQ7D9HG4SCIELNRG", "CH09531981M5F3M84OOA1", "CH3747732IYLZWBIUO7F9", "CR41653976363667206472", "CR61138643870780281035", "CY77181666732W0OK9MO71FP9QFT", "CY4043032144P6VMBCP42EWZZ0GT", "CZ0500701011515222697774", "CZ8485617709412783317386", "DE75323734430247348433", "DE18474192108609592292", "DK8658772182867299", "DK9882231742486007", "DO1342GO71371595187710081568", "DO983FDU20831039568036269792", "EE474480253094078489", "EE847394446572702817", "ES4242461128115519140579", "ES3585913197271535865795", "FI6620464595369114", "FI6656044804506513", "FO2353310063109027", "FO8865860572114390", "FR484635986406YRPB7C37CNQ77", "FR673935332609I6UU9CEY7H298", "GB77QVAA92020908648058", "GB42JSIN88833697784064", "GE21PG3151300222737824", "GE31HW7147961963101845", "GI60EUJGGZ90G5502AYCYBK", "GI62KYZX248197MKNBEG2EQ", "GL3708246420445584", "GL5923657433145030", "GR783797678UQW0XVA4M906IH2G", "GR623988469QO48OEHHLHGJ795T", "GT940IG0SQVBGKLXYNQ0755MZCCL", "GT457IKHTELPTOETQW4QA5DJCB0Z", "HR5696097956129883363", "HR7900029000289927680", "HU35561014514577636317765985", "HU07675568826301397810796833", "IE82MBQF95636985890031", "IE70YAQX91756188770636", "IL061704177385884933485", "IL759658792375469613502", "IQ66OYLR002585034811165", "IQ33AQTG201448577439088", "IS534320914789017766765242", "IS744953893877888517703327", "IT41A2464674442N9UJ58MRV52F", "IT54Q8323578135V15YFILQLCQ2", "JO07HCZN1390P3CFRWO5PFKOZEFYAT", "JO04ROOB747642OUY4VVTEA8E8MF60", "KW87GMKE8596DGCY4BXEEN32TP78AS", "KW20WBZU89RIUV2KF2H295FUZQM0BZ", "KZ728674JK6WEF791XKT", "KZ474218JHQDHFKO8QZ5", "LB5587918EMU483GWSUK5HNDQ3VJ", "LB985311F4XQ0DW6JUCMG71WUC3I", "LC72XICCQJ4L4TXOYHTG7XOKW6R6IUV8", "LC53HMLCAZ05WQ8UBFBARW6AIUL89V2I", "LI57633635O1UBTORXN1W", "LI2478016OPB828VA396N", "LT367811574986624132", "LT107638589265707594", "LU55779IFGB9W9MH1N68", "LU15124SOR3J03LBVPLT", "LV63TTCXKKS6CXOR8K5ZH", "LV27SIATN7ZLFICZ6FIJ0", "MC272584403191YW3PUQ7WKEK64", "MC584147481282OU9HZ3JCLZS70", "MD88OJICR8Z560CV5Z56RLR9", "MD34YDC8NCGPGDHP1FJCLO6P", "ME04836526530797062693", "ME38193344146057837275", "MK27300SMNOTH61BR15", "MK98076F8MQNKSKMK35", "MR7581073934037132495036327", "MR9198356711796580929494537", "MT05OAKX88202ZUV686SRTDLFO3PMOX", "MT86KRMO134227SEERIMR8V7IPN9MMZ", "MU88TMNR0567672225354069719QFY", "MU02BSVW5255797069306607843JQA", "NL15SNSD1856129322", "NL09UYBL7722185460", "NO2342034237064", "NO2168330172255", "PK86JQJC912IQU6L9ZAUNHBE", "PK47QHFQID9HEV74AVAAZVYP", "PL79804386633808801328774446", "PL02479493361858307984715078", "PS87TERJ244J2R389F78COVFK2YYR", "PS72TULCEHPYY6L2X4BJVYZW2GZL9", "PT54925048712726506012121", "PT52731447700575942975818", "QA19CPNM7M6GSJ8RHK193VISJEVV1", "QA96YJPZSRVYGZDBG467C3SNR7LOC", "RO04OYZPMALS0603FQ66DDVN", "RO96VPAB2BEU4WJSMKA2YBS9", "RS85990804997303599384", "RS79691452161895520088", "SA1612FBV017RAOW0FFY6MI4", "SA7852BX0TF16FCT06WXM6QK", "SC21SKQT48397576814218878792BLT", "SC49BHPC24249659100274263645RUC", "SE1427739638040573160054", "SE3555829824039845023616", "SI85263838359592515", "SI94298882643105654", "SK4966726045742512919467", "SK6417545335470924950926", "SM55M9144704930ITBSM6VT6ODJ", "SM74I4990804171TV1CFZN664IA", "ST70025736394519337487434", "ST57928407446221033032677", "SV31BPAF67429031273059240765", "SV37BACK56369176007791803450", "TL309311665903143076228", "TL506496396787421957197", "TN9451047133855297489170", "TN8341827991659205574472", "TR4660960LJBQ0A3BTOJF8GBDH", "TR110858750WLRCLO6F5YL6B8A", "UA95465366MKXGG7M3JB2IAP12IGO", "UA76776931P09ZAM38BKZFDJ7FISO", "VG19HKUD2090704270585150", "VG09IFIZ4245377217696104", "XK522078769488135293", "XK164946249920719752"]

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




                    
ibanTestCases = do 
    print "Testing valid IBANs"
    print $ and $ map iban validIbans
    print "Testing invalid IBANs"
    print $ and $ map not (map iban invalidIbansStatic)
    print "Testing converted IBANs to *probably* invalid IBANs via QuickCheck number increment"
    quickCheckResult(\delta -> (delta /= 0 && mod delta 10 /= 0) --> (and $ map not (map (\code -> ibanTestIncrementSingleNumber code delta) validIbans)))
    print "Testing converted IBANs to *probably* invalid IBANs via QuickCheck single letter transposition"
    quickCheckResult(\p1 -> (and $ map (\code -> ibanTestTranspose code p1) validIbans))




-- ====== Implementing and testing ROT13 encoding
-- time: 60 minutes

{-
ROT13 specification:

1. ROT13(x) =  
    1a) IF x is upper/lowercase letter of latin alphabet, move x 13 positions to the right and wrap around, preserving case
    1b) OTHERWISE x remains unchanged

2. ROT13(ROT13(x)) = x
   ROT13 is its own inverse
    
-}

rot13AlphabetUppercase = ['A'..'Z']
rot13AlphabetLowercase = ['a'..'z']

-- encodes single character using the ROT13 algorithm
rot13SingleChar :: Char -> Char
rot13SingleChar x 
    | x `elem` rot13AlphabetUppercase = rot13Transform (findIndex (== x) rot13AlphabetUppercase) x rot13AlphabetUppercase
    | x `elem` rot13AlphabetLowercase = rot13Transform (findIndex (== x) rot13AlphabetLowercase) x rot13AlphabetLowercase
    | otherwise = x


rot13Transform :: Maybe Int -> p -> [p] -> p
rot13Transform (Just index) x alphabet = alphabet !! ((index + 13) `mod` (length alphabet))
rot13Transform Nothing x _ = x 


rot13 :: String -> String
rot13 input = map rot13SingleChar input

rot13InverseProperty :: String -> Bool
rot13InverseProperty code = (rot13 $ rot13 code) == code

rot13EncodableAlphabet :: String
rot13EncodableAlphabet = rot13AlphabetLowercase ++ rot13AlphabetUppercase 


rot13EncodedProperty :: String -> Bool
rot13EncodedProperty code = and $ map rot13EncodedPropertyChar code
rot13NotEncodedProperty code = and $ map rot13NotEncodedPropertyChar code

rot13EncodedPropertyChar :: Char -> Bool
rot13EncodedPropertyChar x = (x `elem` rot13EncodableAlphabet) --> ((head $ rot13 [x]) /= x)
rot13NotEncodedPropertyChar :: Char -> Bool
rot13NotEncodedPropertyChar x = (not $ x `elem` rot13EncodableAlphabet) --> ((head $ rot13 [x]) == x)


rot13Tests = do 
    quickCheckResult(\x -> rot13InverseProperty x)
    quickCheckResult(\x -> rot13EncodedProperty x)
    quickCheckResult(\x -> rot13NotEncodedProperty x)


-- Recognizing and generating derangements

--Give a Haskell implementation of a property isDerangement that checks whether one list is a derangement of another one.

isDerangement :: [Int] -> [Int] -> Bool
isDerangement a b = (isPermutation a b) && (and $ zipWith (/=) a b)

--Give a Haskell implementation of a function deran that generates a list of all derangements of the list [0..n-1].

deran :: Int -> [[Int]]
deran n = let list = [0..n-1] in filter (\x -> isDerangement x list) (permutations list)


deranCountProperty :: Int -> [[Int]] -> Bool
deranCountProperty n list = subfactorial n == length list


--source: https://codegolf.stackexchange.com/a/113942
subfactorial :: Int -> Int
subfactorial 0 = 1
subfactorial n = n * subfactorial (n-1) + (-1)^n

--time: 43 min

--tests:
derangementTests = do 
    print "Testing that derangement count is the subfactorial"
    quickCheckResult(\n -> (n >= 1) --> ( let x = n `mod` 8 in deranCountProperty x (deran x)))




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





--ibanTestRemove :: String -> Int -> Bool
--ibanTestRemove code pos = let index = (pos `mod` (length code)) in not $ iban (deleteAt index code)
--print "Testing converted IBANs to *probably* invalid IBANs via QuickCheck single character delete"
--quickCheckResult(\p1 -> p1 > 0 --> (and $ map (\code -> ibanTestRemove code p1) validIbans))