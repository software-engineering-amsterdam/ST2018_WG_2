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

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all



-- ====== 1 ======== 1:36 h

freqDistCount list = let 
    q1 = length $ filter (\x -> x > 0 && x < 0.25) list
    q2 = length $ filter (\x -> x >= 0.25 && x < 0.50) list
    q3 = length $ filter (\x -> x >= 0.5 && x < 0.75) list
    q4 = length $ filter (\x -> x >= 0.75 && x < 1) list
    in (q1,q2,q3,q4)


freqDistPerc (a,b,c,d) = let sum = a+b+c+d
                        in (cDiv a sum, cDiv b sum, cDiv c sum, cDiv d sum)

cDiv a b = (fromIntegral a) / (fromIntegral b)

middlePoint = 0.25
tolerance = 0.01

isEvenlyDistributed (a,b,c,d) =  bounded a && bounded b && bounded c && bounded d

bounded x = inRange x (middlePoint - tolerance) (middlePoint + tolerance)
inRange value min max = value >= min && value <= max


testEvenDistributionGenerator = do
        x <- probs 10000
        return (isEvenlyDistributed $ freqDistPerc $ freqDistCount x)


--TODO: how to test with quickcheck

--probs 10000 >>= (\x -> return freqDist x)


-- ========= 3 =======

--data NamedProperty = NamedProperty (String, Int -> Bool)
--instance Show NamedPropertyX where show (NamedProperty name _ ) = show name
--instance Eq NamedProperty where 
--  (NamedPropertyX nameL _) == (NamedPropertyX nameR _) = nameL == nameR



stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 


--a) Implement all properties from the Exercise 3 from Workshop 2 as Haskell functions of type Int -> Bool. Consider a small domain like 

{-
property3_1a = NamedProperty "1a" (\ x -> even x && x > 3) 
property3_1b = NamedProperty "1b" even
--property3_1 = (property3_1a, property3_1b)


property3_2a = NamedProperty "2a" (\ x -> even x || x > 3) 
property3_2b = NamedProperty "2b" even
--property3_2 = (property3_2a, property3_2b)


property3_3a = NamedProperty "3a" (\ x -> (even x && x > 3) || even x) 
property3_3b = NamedProperty "3b" even
--property3_3 = (property3_2a, property3_2b)


property3_4a = NamedProperty "4a" even 
property3_4b = NamedProperty "4b" (\ x -> (even x && x > 3) || even x)

propertyList :: [Int->Bool]
propertyList = [property3_1a, property3_1b, property3_2a, property3_2b, property3_3a, property3_3b, property3_4a, property3_4b]

-}


property3_1a = (\ x -> even x && x > 3) 
property3_1b = even


property3_2a = (\ x -> even x || x > 3) 
property3_2b = even


property3_3a = (\ x -> (even x && x > 3) || even x) 
property3_3b = even


property3_4a = even 
property3_4b = (\ x -> (even x && x > 3) || even x)

propertyList = [property3_1a, property3_1b, property3_2a, property3_2b, property3_3a, property3_3b, property3_4a, property3_4b]


weaknessCompare l r = (stronger domain l r) || ((weaker domain l r) && (stronger domain l r))


domain = [-10..10]

sortBasedOnWeakness [] = []
sortBasedOnWeakness (x:xs) = sortBasedOnWeakness small ++ (x : sortBasedOnWeakness large)
  where small = [y | y <- xs, weaknessCompare y x]
        large = [y | y <- xs, not $ weaknessCompare y x]


--time: 3hr no tests



isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) ys = if x `elem` ys then isPermutation (xs) (deleteOneOccurence ys [] x) else False


deleteOneOccurence [] res _ = res
deleteOneOccurence (x:xs) ys  f = if x == f then (ys++xs) else deleteOneOccurence xs (x:ys) f


equalLength a b = length a == length b
everyElementOfAInB a b = forall a (\x -> elem x b)
everyElementOfBInA a b = everyElementOfAInB b a

permutationTest a b = equalLength a b && everyElementOfAInB a b && everyElementOfBInA a b


--  permutationTest' = permutationTest [1,2,3] [3,2,1] && permutationTest []
--  ([a], [a])



-- =========== IBAN


-- helper functions intToList and listToInt. these functions are each other's 
-- inversions. 
intToList :: Integer -> [Integer]
intToList 0 = [0]
intToList n = (intToList (quot n 10)) ++ [mod n 10]

listToInt :: [Integer] -> Integer
listToInt [] = 0
listToInt l = 
    (last l) + 10 * listToInt (init l)



iban :: String -> Bool
iban code = (ibanValidLength code) && (ibanValidCountry code) && (ibanValidChecksumFormat code) &&  (ibanCalcMod $ ibanSplitIntArray $ ibanTokenizeStringNum $ ibanMoveInitEnd code)

--Check that the total IBAN length is correct as per the country. If not, the IBAN is invalid
--Move the four initial characters to the end of the string
--Replace each letter in the string with two digits, thereby expanding the string, where A = 10, B = 11, ..., Z = 35
--Interpret the string as a decimal integer and compute the remainder of that number on division by 97

--validates that IBAN is within the allowed range
--this function does not check all country specific lengths
ibanValidLength :: String -> Bool
ibanValidLength code = length code >= 15 && length code <= 31

ibanValidCountry code = forall (take 2 code) (\x -> isUpper x && isLetter x) 

ibanValidChecksumFormat code = forall (take 2 $ drop 2 code) isDigit 

--moves initial 4 letters of IBAN to the end
ibanMoveInitEnd :: String -> String
ibanMoveInitEnd code =  let (a, b) = splitAt 4 code in b ++ a

numberOffset :: Integer
numberOffset = - (toInteger $ ord 'A') + 10

--converts numbers to numbers and uppercase letters to the range 10-35
ibanStringExpand :: Char -> Integer
ibanStringExpand letter = if isDigit letter then toInteger $ digitToInt letter else (toInteger $ ord letter) + numberOffset


ibanTokenizeStringNum :: String -> [Integer]
ibanTokenizeStringNum code = map ibanStringExpand code

-- converts numbers >= 10 to their separate digits
-- 1 becomes [1], 1234 becomes [1,2,3,4]
ibanSplitIntArray :: [Integer] -> [Integer]
ibanSplitIntArray [] = []
ibanSplitIntArray (x:xs) = intToList x ++ ibanSplitIntArray xs

-- ISO 7064 number check
ibanCalcMod :: [Integer] -> Bool
ibanCalcMod xs = (listToInt $ map toInteger xs) `mod` 97 == 1


--time: 2:45h

--iban "GB82WEST12345698765432"

validIbans = ["GB82WEST12345698765432", "AD4191688595KBCTGKHRGYZM", "AD4699137644TDVJT9TNAC33"]

--, "AE696216259205408241845", "AL92647917519CIBTHD05U2H8H40", "AL8188806789C75VBDSYFE7JT8RQ", "AT187528538777215120", "AT631500287292735316", "AZ97PSVE6C9IQE5M0JWC8CBRJ5U3", "AZ55DZQEJKL3NZJIB84I4R4AZ2JC", "BA473050619611526256", "BA847034185550629040", "BE02611834831129", "BE38993183209905", "BG78JCEO781881BML7K2AM", "BG29TKCS078680YY8507BU", "BH52MITCVLZI7EKRRMAZZ8", "BH30TPFO1RNKM195AVPRK7", "BR0554129225980945618101435AD", "BR9325178333383395936755257HZ", "BY21BTNJ74378VIB5B1J5YJBUOG7", "BY98LV728466VQ7D9HG4SCIELNRG", "CH09531981M5F3M84OOA1", "CH3747732IYLZWBIUO7F9", "CR41653976363667206472", "CR61138643870780281035", "CY77181666732W0OK9MO71FP9QFT", "CY4043032144P6VMBCP42EWZZ0GT", "CZ0500701011515222697774", "CZ8485617709412783317386", "DE75323734430247348433", "DE18474192108609592292", "DK8658772182867299", "DK9882231742486007", "DO1342GO71371595187710081568", "DO983FDU20831039568036269792", "EE474480253094078489", "EE847394446572702817", "ES4242461128115519140579", "ES3585913197271535865795", "FI6620464595369114", "FI6656044804506513", "FO2353310063109027", "FO8865860572114390", "FR484635986406YRPB7C37CNQ77", "FR673935332609I6UU9CEY7H298", "GB77QVAA92020908648058", "GB42JSIN88833697784064", "GE21PG3151300222737824", "GE31HW7147961963101845", "GI60EUJGGZ90G5502AYCYBK", "GI62KYZX248197MKNBEG2EQ", "GL3708246420445584", "GL5923657433145030", "GR783797678UQW0XVA4M906IH2G", "GR623988469QO48OEHHLHGJ795T", "GT940IG0SQVBGKLXYNQ0755MZCCL", "GT457IKHTELPTOETQW4QA5DJCB0Z", "HR5696097956129883363", "HR7900029000289927680", "HU35561014514577636317765985", "HU07675568826301397810796833", "IE82MBQF95636985890031", "IE70YAQX91756188770636", "IL061704177385884933485", "IL759658792375469613502", "IQ66OYLR002585034811165", "IQ33AQTG201448577439088", "IS534320914789017766765242", "IS744953893877888517703327", "IT41A2464674442N9UJ58MRV52F", "IT54Q8323578135V15YFILQLCQ2", "JO07HCZN1390P3CFRWO5PFKOZEFYAT", "JO04ROOB747642OUY4VVTEA8E8MF60", "KW87GMKE8596DGCY4BXEEN32TP78AS", "KW20WBZU89RIUV2KF2H295FUZQM0BZ", "KZ728674JK6WEF791XKT", "KZ474218JHQDHFKO8QZ5", "LB5587918EMU483GWSUK5HNDQ3VJ", "LB985311F4XQ0DW6JUCMG71WUC3I", "LC72XICCQJ4L4TXOYHTG7XOKW6R6IUV8", "LC53HMLCAZ05WQ8UBFBARW6AIUL89V2I", "LI57633635O1UBTORXN1W", "LI2478016OPB828VA396N", "LT367811574986624132", "LT107638589265707594", "LU55779IFGB9W9MH1N68", "LU15124SOR3J03LBVPLT", "LV63TTCXKKS6CXOR8K5ZH", "LV27SIATN7ZLFICZ6FIJ0", "MC272584403191YW3PUQ7WKEK64", "MC584147481282OU9HZ3JCLZS70", "MD88OJICR8Z560CV5Z56RLR9", "MD34YDC8NCGPGDHP1FJCLO6P", "ME04836526530797062693", "ME38193344146057837275", "MK27300SMNOTH61BR15", "MK98076F8MQNKSKMK35", "MR7581073934037132495036327", "MR9198356711796580929494537", "MT05OAKX88202ZUV686SRTDLFO3PMOX", "MT86KRMO134227SEERIMR8V7IPN9MMZ", "MU88TMNR0567672225354069719QFY", "MU02BSVW5255797069306607843JQA", "NL15SNSD1856129322", "NL09UYBL7722185460", "NO2342034237064", "NO2168330172255", "PK86JQJC912IQU6L9ZAUNHBE", "PK47QHFQID9HEV74AVAAZVYP", "PL79804386633808801328774446", "PL02479493361858307984715078", "PS87TERJ244J2R389F78COVFK2YYR", "PS72TULCEHPYY6L2X4BJVYZW2GZL9", "PT54925048712726506012121", "PT52731447700575942975818", "QA19CPNM7M6GSJ8RHK193VISJEVV1", "QA96YJPZSRVYGZDBG467C3SNR7LOC", "RO04OYZPMALS0603FQ66DDVN", "RO96VPAB2BEU4WJSMKA2YBS9", "RS85990804997303599384", "RS79691452161895520088", "SA1612FBV017RAOW0FFY6MI4", "SA7852BX0TF16FCT06WXM6QK", "SC21SKQT48397576814218878792BLT", "SC49BHPC24249659100274263645RUC", "SE1427739638040573160054", "SE3555829824039845023616", "SI85263838359592515", "SI94298882643105654", "SK4966726045742512919467", "SK6417545335470924950926", "SM55M9144704930ITBSM6VT6ODJ", "SM74I4990804171TV1CFZN664IA", "ST70025736394519337487434", "ST57928407446221033032677", "SV31BPAF67429031273059240765", "SV37BACK56369176007791803450", "TL309311665903143076228", "TL506496396787421957197", "TN9451047133855297489170", "TN8341827991659205574472", "TR4660960LJBQ0A3BTOJF8GBDH", "TR110858750WLRCLO6F5YL6B8A", "UA95465366MKXGG7M3JB2IAP12IGO", "UA76776931P09ZAM38BKZFDJ7FISO", "VG19HKUD2090704270585150", "VG09IFIZ4245377217696104", "XK522078769488135293", "XK164946249920719752", "YY23VRDP82747688KNWMMBPHXU2VS707M7", "YY65DJIN40964266R4HH4IMYJP31TOBY0J", "ZZ39OLGD242955957BN4D7IG7DF4YY0RNBP", "ZZ13HPJE2132771329GYJYXDDZ686MN1K3Y"]
