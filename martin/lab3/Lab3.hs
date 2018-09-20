module Lab3 where
 
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all



contradiction :: Form -> Bool
contradiction formula = forall (allVals formula) (\val -> not $ (evl val formula))

tautology :: Form -> Bool
tautology formula = forall (allVals formula) (\val -> (evl val formula))


-- | logical entailment 
entails :: Form -> Form -> Bool
entails formulaA formulaB = forall (filter (\value -> evl value formulaB) (allVals formulaB)) (\value -> evl value formulaA )


entails' formulaA formulaB = and $ zipWith (\b a -> evl b formulaB --> evl a formulaA ) (allVals formulaB) (allVals formulaA)

--TODO: TEST ME

-- | logical equivalence
-- A |= B && B |= A --> A <=> B
equiv, equiv' :: Form -> Form -> Bool
equiv formulaA formulaB = entails formulaA formulaB && entails formulaB formulaA

equiv' formulaA formulaB = 
    let values = allVals formulaA in 
        and $ zipWith (\a b -> a == b) --TODO: see if tests catch swapping == with &&
        (map (\val -> (evl val formulaA)) values)
        (map (\val -> (evl val formulaB)) values)
    
--TODO: TEST ME


-- ex 1 no tests: 1hour



-- ex 4

data FormGenConnectives 
    = Literal | Not | And | Or | Implies | Equivalent 
    deriving (Eq,Ord, Enum, Show)


--source: https://stackoverflow.com/a/25924694

generateConnectiveValues :: (Enum a) => [a]
generateConnectiveValues = enumFrom (toEnum 0)

connectivesList :: [FormGenConnectives]
connectivesList = generateConnectiveValues

--form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)

--andFormulaGenerator :: Int -> [Int] -> [FormGenConnectives] -> Form
--andFormulaGenerator maxConnectives
--    | (maxConnectives <= 0) = Prop 1
--    | otherwise = Cnj [
--        (andFormulaGenerator ((maxConnectives-1) `quot` 2)),
--        (andFormulaGenerator ((maxConnectives-1) `quot` 2)) 
--        ]



--formulaGenerator :: Int -> [Int] -> [FormGenConnectives] -> Form
--formulaGenerator maxConnectives paramOut (connective:connectives)
--    | maxConnectives <= 0 = Prop connective
--    | otherwise = 


--quickCheckResult(\literals connectives -> equivalence ourformula jellesCNF(ourformula))

formulaGenerator :: [Int] -> [Int] -> Form
formulaGenerator literalsNum connectivesNum = 
    let literalsNumAbs = map abs literalsNum
        connectivesNumMod = map (\x -> (abs x) `mod` (length connectivesList)) connectivesNum
        connectivesEnum = map toEnum  connectivesNumMod
    in formulaGenerator' literalsNumAbs connectivesEnum


--base case: next step has no more literals to output OR connectives

formulaGenerator' :: [Int] -> [FormGenConnectives] -> Form
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


--TODO: how to check?

--time: 2:25h


--source: https://stackoverflow.com/a/19074708
splitHalf :: [a] -> ([a],[a])
splitHalf l = splitAt ((length l + 1) `div` 2) l
