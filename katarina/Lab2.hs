
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


-- Exercise 1 Are the numbers generated randomly?
--areInTheFirstQuarter :: 
randomNumbers n = probs n

areInTheFirstQuartile = filter (\x -> x < 0.25 && x>0) 



---Exercise 3 First Part
-- Implement the properties
firstPropertyLeft, secondPropertyLeft, thirdPropertyLeft, fourthPropertyLeft :: Int -> Bool 
firstPropertyRight, secondPropertyRight, thirdPropertyRight, fourthPropertyRight :: Int -> Bool 


--firstPropertyLeft x = even x && x > 3  
--firstPropertyRight x = even x
--secondPropertyLeft x = even x || x > 3 
--secondPropertyRight x = even x
--thirdPropertyLeft x = even x && x > 3 || even x
--thirdPropertyRight x = even x
--fourthPropertyLeft x = even x 
--fourthPropertyRight x = even x && x > 3 || even x

firstPropertyLeft = (\x -> even x && x > 3)
firstPropertyRight = even
secondPropertyLeft  = (\x ->even x || x > 3 )
secondPropertyRight  = even
thirdPropertyLeft  = (\x -> even x && x > 3 || even x)
thirdPropertyRight  = even 
fourthPropertyLeft  = even 
fourthPropertyRight = (\x ->even x && x > 3 || even x)

--listOfAllProperties x = [firstPropertyLeft x, firstPropertyRight x, secondPropertyLeft x, secondPropertyRight x,thirdPropertyLeft x, thirdPropertyRight x, fourthPropertyLeft x, fourthPropertyRight x]
listOfAllProperties = [firstPropertyLeft , firstPropertyRight, secondPropertyLeft, secondPropertyRight ,thirdPropertyLeft, thirdPropertyRight, fourthPropertyLeft, fourthPropertyRight]
--Provide a list of descending strength
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

--quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
   quicksort [ a | a <- xs, weaker [-10..10] a x ]
   ++ [x]
   ++ quicksort [ a | a <- xs, stronger [-10..10] a x ]



--Exercie 3 Second Part
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) ys = if x `elem` ys then isPermutation (xs) (deleteOneOccurence ys [] x) else False

deleteOneOccurence [] res _ = res
deleteOneOccurence (x:xs) ys  f = if x == f then (ys++xs) else deleteOneOccurence xs (x:ys) f

--Test properties
equalLength a b = length a == length b
everyElementOfAInB a b = forall a (\x -> elem x b)
everyElementOfBInA a b = everyElementOfAInB b a

permutationTest :: (Eq a) => [a]->[a]->Bool
permutationTest a b = equalLength a b && everyElementOfAInB a b && everyElementOfBInA a b


--Next, define some testable properties for this function, and use a number of well-chosen lists to test isPermutation. You may assume that your input lists do not contain duplicates. 
--What does this mean for your testing procedure?
permutationTest' = permutationTest [1,2,3,4,5] [5,3,4,2,1] && permutationTest [1,2,3] [3,2,1] && permutationTest [1] [1] 
  && permutationTest [1,2,3] [3,2,2,3]
--  && permutationTest [] [] 


--Provide an ordered list of properties by strength using the weakear and stronger definitions.



-- For quick check we cant use the generator for both lists to compare. We will use the generator for one list and manipulate this list to compare. Two versions swap and reverse
swap2 :: [a]->[a]
swap2 [] = []
--swap2 (x:[]) = [x]
swap2 [x] = [x]
swap2 (x:y:xs)  = y:x:(swap2 xs)

reverseList :: [a] -> [a]
reverseList [] = []
reverseList [x] = [x]
reverseList (x:xs) = reverseList xs ++ [x]

--Can you automate the test process? Use the techniques presented in this week's lecture. Also use QuickCheck.
-- We want to be sure that the function is wrong if we dont have a permutation. Therefor we test two lists of the same length but with different elements
quickCheckPermutationSwap a = permutationTest a (swap2 a) 
quickCheckPermutationReverse a = permutationTest a (reverseList a)
quickCheckPermutationNotEqual a = permutationTest a (map (\x -> x+10) a)


--===========================================
--Exercise 6 IBAN
--===========================================
-- Time spent: ~ 5h 
intToList :: Int -> [Int]
intToList 0 = []
intToList n = (intToList (quot n 10)) ++ [mod n 10]

listToInt :: [Integer] -> Integer
listToInt [] = 0
listToInt l = 
    (last l) + 10 * listToInt (init l)

splitIBAN :: [Int] -> [Int]
splitIBAN [] = [] 
splitIBAN (0:xs) = [0] ++ splitIBAN xs
splitIBAN (x:xs) = intToList x ++ splitIBAN xs

listToIntIBAN :: [Int] -> Integer
listToIntIBAN iban = listToInt $ map toInteger iban






changeOrder::(a,b)->(b,a)
changeOrder (a,b) = (b,a)

toString (a,b) = a++b




-- function transforms calls reorder and then replaces, afterwards numbers with 2 digits, such as 13,728 are split. Afterwards the list of digits is formed into one Integer. Integer not int because number would be too long
transform::String->Integer
transform iban = listToIntIBAN $ splitIBAN $ replaceCharByNum (reorder iban)

reorder::String->String
reorder iban = toString $ changeOrder $ splitAt 4 iban --returns a String containing the reordered IBAN

replaceCharByNum :: String -> [Int]
replaceCharByNum iban = map (\x -> (if isDigit x then digitToInt x else ((ord x) - 55))) iban






ibanLenght a = length a <= 32 && length a >= 15




-- Final Check
iban :: String -> Bool
iban ibanNumber = (transform ibanNumber)  `mod`  97 == 1 && ibanLenght ibanNumber



---Test, ibans generated by mobildefish.com
ibanGenerated = ["AD8292382096591534422641", "AE198914306811033428606", "AL14309139789427700518072509", "AT285773913923072016", "AZ54AZAD17557331686866082764", "BA703240553341903206", "BE26567739296699", "BG85HOWB16877937901071", "BH92JEOR88558045451299", "BR0231013804936747287158667H2", "BY69898855201908955039937657", "CH2040577691166579243", "CR02032896829975825380", "CY67067425555710689739252874", "CZ3817321628944366894303", "DE93380692479886195738", "DK3908824851098094", "DO82987539586686486977900999", "EE178594956294158792", "ES3998717771432640336536", "FI4741112734650161", "FO0546765686363560", "FR6808358622078734322949643", "GB48VDQE34576370374371", "GE53PW0383849580345125", "GI54AVYA931321659508564", "GL8687324907852236", "GR2678711444962014339665334", "GT42274975380181774266744179", "HR4513020110871078092", "HU05456475061865635056186458", "IE27QNWO41802297061999", "IL699575126920525449989", "IQ32JPJQ342296281472931", "IS904449404537866648787797", "IT42Z8341456753624647742203", "JO74PCZO0929952175686271750507", "KW81RAVN5243804166761402461640", "KZ576453435058355778", "LB20329794541528958956355480", "LC77UIDA183654799228617172292076", "LI5861233836242215101", "LT532986776158738847", "LU301577116196319687", "LV14WGQW4831476169414", "MC6644304164462202505581986", "MD3044547435689208374117", "ME52756622236318400069", "MK63306725299982371", "MR6334176080003847350873911", "MT67IHST24129567778948599762289", "MU94YVVP9402849134148546465LZH", "NL26ZMAP1215240937", "NO8330715411423", "PK28YWDS2388734727322997", "PL67050665141630113959694633", "PS63NXHT066196875465049821036", "PT44779218271881877312886", "QA71ANMW098214806663620176644", "RO32ITFI5726137771576363", "RS23430543138091431083", "SA5724929173402932809249", "SC83OQNE94331545058695106903HKV", "SE4329979887862626561558", "SI74093497099793263", "SK1960279093047070476695", "SM42Z4073554256570618547031", "ST86450361964463675555313", "SV75RAJM55560918436619881231", "TL473658770406505687589", "TN8017103587235903044758", "TR526832044844445002085800", "UA465065952319960330224952986", "VG80EHYS1469116098984745", "XK507961512988674206"]



ibansAreValid = filter (\x -> not (iban x)) ibanGenerated

-- We don't want to change the country code or the checksum, so we change the first two pairs each, but just the digits after the first four digits
swapNTimes _ [] = []
swapNTimes _ [x] = [x]
swapNTimes 0 xs = xs
swapNTimes n (x:y:xs) = y:x:(swapNTimes (n-1) xs)

ibanSpecSwap :: [a] -> [a]
ibanSpecSwap [] = []
ibanSpecSwap (a:b:c:d:xs) = a:b:c:d:(swapNTimes 2 xs)

ibansAreInvalid = filter (\x -> iban x) (map (\y -> ibanSpecSwap y) ibanGenerated)

--No we are not allowed to produce ibans, see Wikipedia
-- According to the ECBS "generation of the IBAN shall be the exclusive responsibility of the bank/branch servicing the account"