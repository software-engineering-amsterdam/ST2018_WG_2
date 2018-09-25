module Lab3

where 

import Data.List
import Test.QuickCheck
import Data.Char
import Data.String
import Lecture3
import System.Random


forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- exercise 1
contradiction :: Form -> Bool
contradiction formula = forall (allVals formula) (\val -> not $ (evl val formula))

tautology :: Form -> Bool
tautology formula = forall (allVals formula) (\val -> (evl val formula))

entails, entails' :: Form -> Form -> Bool
entails formulaA formulaB = forall (filter (\value -> evl value formulaB) (allVals formulaB)) (\value -> evl value formulaA )

entails' formulaA formulaB = and $ zipWith (\b a -> evl b formulaB --> evl a formulaA ) (allVals formulaB) (allVals formulaA)


equiv :: Form -> Form -> Bool
equiv formulaA formulaB = entails formulaA formulaB && entails formulaB formulaA

-- exercise  2:
--compare 2 Strings with equivalence, generate Formular (Form) with generator (exercise 4) -> convert to String (show)-> parse and check equivalence

maxNum :: Int --determines the maximun number of literals and connectives
maxNum = 20

checkEquivalance n = equiv formula (head(parse (show (formula))))
  where formula= formulaGenerator [0..((abs n) `mod` maxNum)] [0..((abs n) `mod`maxNum)]

  -- Test:  quickCheck checkEquivalence

-- exercise 3
-- conversion of formulas into CNF

-- exercise 4

data FormGenConnectives 
    = Literal | Not | And | Or | Implies | Equivalent 
    deriving (Eq,Ord, Enum, Show)


--source: https://stackoverflow.com/a/25924694
generateEnumValuesGeneric  :: (Enum a) => [a]
generateEnumValuesGeneric  = enumFrom (toEnum 0)

connectivesList :: [FormGenConnectives]
connectivesList = generateEnumValuesGeneric


formulaGenerator :: [Int] -> [Int] -> Form
formulaGenerator literalsNum connectivesNum = 
    let literalsNumAbs = map abs literalsNum
        connectivesNumMod = map (\x -> (abs x) `mod` (length connectivesList)) connectivesNum
        connectivesEnum = map toEnum  connectivesNumMod
    in formulaGenerator' literalsNumAbs connectivesEnum


--base case: next step has no more literals to output OR connectives

formulaGenerator' :: [Int] -> [FormGenConnectives] -> Form
formulaGenerator' [] _ = error "Out of connectives while attempting to output a connective"
formulaGenerator' (literal:literals) connectives
    | (null literals || null connectives) = Prop literal


formulaGenerator' (literal:_) (Literal:connectives) = Prop literal


--decrease connectives, keep literals, half of literals required on each side
formulaGenerator' literals (And:connectives) = 
    let (left, right) = formulaGeneratorSplit literals connectives in 
    Cnj [left, right] 
    
formulaGenerator' literals (Or:connectives) =
    let (left, right) = formulaGeneratorSplit literals connectives in 
    Dsj [left, right]  

formulaGenerator' literals (Implies:connectives) =
    let (left, right) = formulaGeneratorSplit literals connectives in 
    Impl left right  

formulaGenerator' literals (Equivalent:connectives) =
    let (left, right) = formulaGeneratorSplit literals connectives in 
    Equiv left right

--decrease connectives, keep literals
formulaGenerator' literals (Not:connectives) = Neg left 
    where
    left = formulaGenerator' literals connectives

formulaGeneratorSplit :: [Int] -> [FormGenConnectives] -> (Form, Form)
formulaGeneratorSplit literals connectives = (left, right) 
    where
    halfListLiterals = splitHalf literals
    halfListConnectives = splitHalf connectives
    left = formulaGenerator' (fst halfListLiterals) (fst halfListConnectives)
    right = formulaGenerator' (snd halfListLiterals) (snd halfListConnectives)



--time: 2:25h


--source: https://stackoverflow.com/a/19074708
--splits list into almost even halves, with left side being larger by 1 if uneven
splitHalf :: [a] -> ([a],[a])
splitHalf l = splitAt ((length l + 1) `div` 2) l