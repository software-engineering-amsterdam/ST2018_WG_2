
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
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

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- getIndex :: (Eq a) => [a] -> a -> Int
-- getIndex l x = getIndex' l x 0

-- getIndex' [] _ _ = -1
-- getIndex' (a:as) x n = 
--     if a == x then n else getIndex' as x n+1

-- ======================
-- == Red Curry floats == 1:30:00
-- ======================

probsTest = do
    print "probsTest"
    randomFloats <- probs 10000
    return (probsTest' randomFloats)

-- probsTest' :: Fractional a => [Float] -> a
probsTest' randomFloats = 
    let rangeCounts = countRanges randomFloats (0,0,0,0)
        validity = chiSquared rangeCounts (length randomFloats)
    in validity

-- chiSquared :: Fractional a => (Int,Int,Int,Int) -> Int -> a
chiSquared (a,b,c,d) n = 
    let e = (fromIntegral n) / 4.0
        values = (a:b:c:d:[])
        chiSquaredValues = map (\x -> ((x - e)^2)/e) values
    in sum chiSquaredValues

-- countRanges :: [Float] -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
countRanges [] (a,b,c,d) = (a,b,c,d)
countRanges (x:xs) (a,b,c,d)
    | x < 0.25 = countRanges xs (a+1,b,c,d)
    | x < 0.5  = countRanges xs (a,b+1,c,d)
    | x < 0.75 = countRanges xs (a,b,c+1,d)
    | x < 1.0  = countRanges xs (a,b,c,d+1)

-- ===========================
-- == Recognizing Triangles == 1:30:00
-- ===========================
-- helper function to convert a list of length 3 to a tuple
listToTuple :: [Int] -> (Int,Int,Int)
listToTuple (a:b:c:[]) = (a,b,c)

-- helper function to get random integers in the domain [low, high]
randInt' :: Int -> Int -> IO Int
randInt' low high = do
    value <- getStdRandom (randomR (low, high))
    if True then return value else return 0

randInt :: Int -> Int -> Int
randInt a b = unsafePerformIO (randInt' a b)

triangle :: Int -> Int -> Int -> Shape
triangle a' b' c' = 
    -- we start by sorting the integers so that we know that a <= b <= c
    let (a,b,c) = listToTuple (sort [a',b',c'])
    in 
        -- if the two shorter sides don't reach the longer one, no triangle
        if a + b <= c then NoTriangle else
            -- if all sides are equally long, equilateral
            if (a == b) && (b == c) then Equilateral else
                -- if we have no equilateral, but two sides are equal, isoceles
                if (a==b) || (b==c) || (a==c) then Isosceles else
                    -- if we found a pythagorean triplet, rectangular. else: other.
                    if (a^2 + b^2 == c^2) then Rectangular else Other

-- perform five different tests with custom randint generators and compare
-- them all to find whether our triangle function is valid.
triangleTest = noTriangleTest && equilateralTest && isocelesTest
                && rectangularTest && otherTest

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

-- =================================
-- == Testing Properties Strength == 1:00:00
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

sortWith :: (a -> a -> Bool) -> [a] -> [a]
sortWith _ [] = []
sortWith f (x:xs) = sortWith f [i | i <- xs, f i x] ++ [x] 
                    ++ sortWith f [i | i <- xs, not (f i x)]

sortedProps :: [[Char]]
sortedProps = 
    let contenders = [("prop1", prop1), ("prop2", prop2), ("prop3", prop3), ("prop4", prop4)]
        result = sortWith stronger contenders
    in map (\(x,_) -> x) result

-- ==============================
-- == Recognizing Permutations == 1:00:00
-- ==============================
isPermutation :: [Int] -> [Int] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) y =
    if not $ elem x y then False else 
        isPermutation xs (filter (/= x) y)

-- =============================================
-- == Recognizing and Generating Derangements == 1:00:00
-- =============================================
-- only consider the two lists if their sorted versions are equivalent to each
-- other, this checks if the two lists contain the same elements. If this is
-- not the case, they cannot be derangements of each other.
isDerangement :: [Int] -> [Int] -> Bool
isDerangement x y = if (sort x) == (sort y) then isDerangement' x y else False

isDerangement' :: [Int] -> [Int] -> Bool
isDerangement' [] [] = True
isDerangement' (x:xs) (y:ys) = if x == y then False else isDerangement' xs ys

deran :: Int -> [[Int]]
deran n = 
    let (x:xs) = permutations [0..n-1] 
    in filter (isDerangement x) xs

-- we restrict the domain of n to [1,20) so that the result remains computable
isDerangementTest :: Int -> Bool
isDerangementTest n = apply (n <= 1 && n > 10) (-->) (and (map (isDerangement [0..n-1]) (deran n)))

apply :: a -> (a -> b -> c) -> b -> c
apply a f b = f a b

deranPreCond :: [Int] -> Bool
deranPreCond l = length l > 1

deranPostCond :: [Int] -> [Int] -> Bool
deranPostCond x y = isDerangement x y

-- =============
-- === ROT13 ===
-- =============
{-
rot13 takes a letter in Character form, i.e. an ASCII character in the domain
[65,90]v[97,122]. It will add 13 to this number and if that causes it to be
outside of the domain subtract 26 to bring it back into the domain of letters.
-}
isLetter' :: Int -> Bool
isLetter' x = elem x ([ord 'a'..ord 'z']++[ord 'A'..ord 'Z'])

rot13 :: [Char] -> [Char]
rot13 [] = []
rot13 (x:xs) = (rotate x : rot13 xs)

rotate :: Char -> Char
rotate x = 
    if isLetter x then
        let value = fromEnum x + 13
            newValue = if isLetter' value then value else value-26
        in toEnum newValue
    else x


-- ============
-- === IBAN ===
-- ============
ibanTest = do
    print "valid for"
    print (length (filter (== True) (map validIBAN ibanList)))
    print "out of"
    print (length ibanList)
    print "Instances..."

validIBAN :: String -> Bool
validIBAN iban = elem (length iban) [15..34] && 
                 firstTwoLetters iban &&
                 moduloTest iban

firstTwoLetters :: String -> Bool
firstTwoLetters string = and (map (\x -> (isLetter x && isUpper x)) (take 2 string))

-- checks for valid IBAN according to ISO 7064 modulo check and IBAN format rules
-- It first moves the first four digits to the end, then prceeds with the calculation.
-- If the result of the calculation has mod 97 of zero, our IBAN is valid. 
moduloTest :: String -> Bool
moduloTest iban = 
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


ibanList = ["AD9471872522635875393618","AE117396106094104187111","AL80769392401490931862006216","AT327306685433338704","AZ04QXGK06192053279137096266","BA175192157268660878","BE49938307672568","BG17TPWI40463360933100","BH33JWKT44954677159897","BR3354394464872020923804187J6","BY68406857740997287111011036","CH2659789404337716381","CR71645720790165172150","CY62718047284461679452357046","CZ9224387041911595440085","DE61601397569122801851","DK2445921903212092","DO83127109282056186918376507","EE111556365564813925","ES9663454530033995413827","FI2989153636280931","FO2878431530292821","FR6429699449785458659042362","GB83DFSG98467344056628","GE87WH5394322814722208","GI13PITT191529923678818","GL0667513415546284","GR5697403954409431130026929","GT35640788242565875851590462","HR7606335476356050562","HU80483301333245057443530305","IE56RIOB87279482445322","IL115234849245785275794","IQ95ZACT499795346555473","IS055692712554180580555969","IT89X9462768326520803064041","JO61WHXS1297586383655166361793","KW35BTDJ2740469976903882768725","KZ658055831502330301","LB85479520773516829077403972","LC39IFYG261644855045445818137175","LI3901882452324734885","LT720712861898064781","LU856199785584112866","LV80AIGR3256034449000","MC9480770064237996604587885","MD4924743478695562603919","ME77117721029823851678","MK48536341807806163","MR2613511219679479098892024","MT73GVEG06238004028343657373147","MU55UNCI4362823109441414181CAP","NL23DRPY6336775909","NO5757569065812","PK52OYGD6008856998759621","PL88255983888497597741161287","PS97IDTP713633264893843437501","PT52806920799498138769839","QA45PNQM696656755980751638540","RO25TPJB7888444046258498","RS41428138319644314293","SA0232529869807575224444","SC60JOLR32063789959923197808LAI","SE9716836145315103430320","SI27574381719836027","SK8418901743690820708789","SM39S3164144277527426106679","ST91783895954024023376282","SV32BMMR89882131613269281553","TL212200615131456475852","TN1774378748138728053443","TR279357764029250532612102","UA536487999099827061917779059","VG34YCET4746247572873663","XK422533381379757806","YY16QNZY55988409238500399484199102","ZZ15PRXE128184242000279808943221766"]

-- ===============
-- === EULER 1 ===
-- ===============
-- apply the filter to the search space, outcome is 233168
sumOf35Dividers :: Int
sumOf35Dividers = sum (filter divBy3or5 [1..999])

divBy3or5 :: Int -> Bool
divBy3or5 x = mod x 3 == 0 || mod x 5 == 0

-- ===============
-- === EULER 4 ===
-- ===============
-- helper function to decide whether a number is a palindrome
palindrome :: Int -> Bool
palindrome x = read (reverse (show x)) == x

-- generator for all palindromes that is the product of two three-digit numbers
threeDigitProductPalindromes :: [(Int,Int,Int)]
threeDigitProductPalindromes = [(x,y,z) | x <- [100..999], y <- [x..999], let z = x*y, palindrome z]

-- function that sorts tuples on the basis of their third element
thirdTupleBigger :: (Int,Int,Int) -> (Int,Int,Int) -> Bool
thirdTupleBigger (_,_,a) (_,_,b) = a > b

-- get the list with all threeDigitPalindromes, sort it in descending order, and take 
-- the head to find the answer: 913 times 993 = 906609
largestThreeDigitProductPalindrome = head (sortWith thirdTupleBigger threeDigitProductPalindromes)

-- ===============
-- === EULER 5 ===
-- ===============
divisibleByRange :: Integer -> Bool
divisibleByRange n = divisibleByRange' n [11..20]

divisibleByRange' :: Integer -> [Integer] -> Bool
divisibleByRange' _ [] = True
divisibleByRange' n (x:xs) = (mod n x == 0) && (divisibleByRange' n xs)

-- Answer: 232792560
smallestDivisibleThrough20 :: Integer
smallestDivisibleThrough20 = head [x | x <- [20,30..], divisibleByRange x]