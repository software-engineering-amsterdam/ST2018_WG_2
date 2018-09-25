module Lab3 

where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- =========================
-- == 1: Formula Analysis == 60 mins
-- =========================

-- helper function that evaluates all valuations for a given formula by applying 
-- them to said f. The resulting list is then a truth-table for this formula
solutions :: Form -> [Bool]
solutions f = map (\x -> evl x f) (allVals f)

-- the formula is a contradiction if the truth table contains no True Bools
contradiction :: Form -> Bool
contradiction f = not (or (solutions f))

-- the formula is a contradiction if the truth table contains only True Bools
tautology :: Form -> Bool
tautology f = and (solutions f)

-- f1 entails f2 if the truth table of f1 entails the truth table of f2
entails :: Form -> Form -> Bool
entails f1 f2 = and (zipWith (-->) (solutions f1) (solutions f2))

-- f1 is equivalent to f2 if f1 entails f2 and f2 entails f1
equiv :: Form -> Form -> Bool
equiv f1 f2 = entails f1 f2 && entails f1 f2

test1 = do
    print "Testing the functions checking formula types..."
    print "Checking if (A ^ not(A)) is a contradiction..."
    print (contradiction (Cnj [Prop 1, (Neg (Prop 1))]))
    print "Checking if (A v not(A)) is a tautology..."
    print (tautology (Dsj [Prop 1, (Neg (Prop 1))]))
    print "Checking if (A ^ B) entails A..."
    print (entails (Cnj [(Prop 1), (Prop 2)]) (Prop 1))
    print "Checking if (A -> B) is equivalent to not(A) or B..."
    print (equiv (Impl (Prop 1) (Prop 2)) (Dsj [(Neg (Prop 1)), (Prop 2)]))
    print "If all functions returned True, test is passed"
    

-- ============================
-- == 2: Parse Function Test == 45 minutes
-- ============================
{-
To test the parse function, we use our homemade formulagenerator. If we generate a formula instantiated by random values provided by quickcheck, then convert this formula to a string using the provided show implementation, we can parse the string and see if the result is equivalent to our original. We convert the lists of Ints provided by quickcheck to lists of length at least one (Our formula generator doesn't like empty lists) and at most 20 (to preserve computability in acceptable time). If at any point the parser gives a result list of length other than 1, this means either our formula generator messed up or the parser function is broken. If we ever encounter a set of formulas that are not equivalent, this also means something is not right. But, the only way we encounter only passing tests, is if our generator and parser are both correct. Using the query:
    " $> quickCheck parseTester "
We get the result:
    " +++ OK, passed 100 tests. "
which means our implementation is valid in at elast 100 random cases.
-}
parseTester :: [Int] -> [Int] -> Bool
parseTester atoms connectives = 
    let atomsAct = if length atoms /= 0 then take 20 atoms else [1]
        connectivesAct = if length connectives /= 0 then take 20 connectives else [1]
        generatedFormula = formulaGenerator atomsAct connectivesAct
        parsedFormula = parse $ show generatedFormula
    in if length parsedFormula /= 1 then False
        else equiv (head parsedFormula) generatedFormula

-- ====================================
-- == 3: Conjunctive Normal Function == 6 hours
-- ====================================

-- Approach is inspired by https://www.cs.jhu.edu/~jason/tutorials/convert-to-CNF.html
-- Tried to implement this algorithm in Haskell, this works in some cases but as soon as the function become deeper and the function has to recurse more than two layers it des not work properly. This is because the function doesn't handle multiple conjunction and disjunction reassembly well.

isConjNormForm :: Form -> Bool
isConjNormForm (Cnj listOfClauses) = and (map isClause listOfClauses)
isConjNormForm f = isClause f || isLiteral f

isClause :: Form -> Bool
isClause (Dsj listOfLiterals) = and (map isLiteral listOfLiterals)
isClause f = isLiteral f

isLiteral :: Form -> Bool
isLiteral (Prop _) = True
isLiteral (Neg (Prop _)) = True
isLiteral _ = False

-- helper function to determine whether a Formula is a top-level Conjunction
isCnj :: Form -> Bool
isCnj (Cnj _) = True
isCnj _ = False

-- helper function to combine multiple conjunctions into one
cnjCombiner :: Form -> [Form] -> Form
cnjCombiner (Cnj acc) [] = (Cnj acc)
-- cnjCombiner (Cnj acc) ((Cnj x):xs) = cnjCombiner (Cnj (acc ++ x)) xs 
cnjCombiner (Cnj acc) (x:xs) 
    | isCnj x = cnjCombiner (Cnj (acc ++ [x])) xs
    | isLiteral x = cnjCombiner (Cnj (acc ++ [x])) xs

-- heper function that loops over all the forms in a list and packs all the literals in a Cnj
cnjParse :: [Form] -> [Form]
cnjParse f = map (\x -> if isLiteral x then (Cnj [x]) else x) f

-- helper function that appllies the law of distribution of disjunction over its argument. Our argument f is of the form [Cnj [Form]]. If we take two possible indexes from the list, choose two conjunctions with list of formulas, then we can compose a list of disjunctions by choosing one formula from one conjunction list each and putting this in a Disjunction.
dsjDistributor :: [Form] -> [Form]
dsjDistributor unparsed_f = 
    let f = cnjParse unparsed_f
    in [ (Dsj [x,y]) | 
    n <- [length f], i1 <- [0..n-1], i2 <- [i1+1 .. n-1],
    (Cnj c1) <- [f !! i1], (Cnj c2) <- [f !! i2],
    x <- c1, y <- c2 ]

-- Convert into Conjunctive Normal Form. This function executes a single iteration of converter functions over the Formula.
convertIntoCNF :: Form -> Form

-- A singular property doesn't need to be converted, we can jsut return the argument
convertIntoCNF (Prop x) = (Prop x)

-- if we convert all elements of a conjunction, they must all be in conjunctive form. So if we combine the results of the conversion into one big conjunction, we will have converted the formula.
convertIntoCNF (Cnj f) = cnjCombiner (Cnj []) (map convertIntoCNF f)

-- the disjunction converter needs to convert all of its arguments, these will then be conjunctions. We are then in the form (Dsj [(Cnj [Form])]). From this we will have to apply the law of distribution of Disjunctions, our result is in CNF.
convertIntoCNF (Dsj f) = (Cnj (dsjDistributor (map convertIntoCNF f)))

-- converter for all negated formulas. If the negated Form is an atom, we are done
convertIntoCNF (Neg (Prop x)) = (Neg (Prop x))

-- the negation of a negation is itself again, we do need to convert the rest though
convertIntoCNF (Neg (Neg x)) = convertIntoCNF x

-- negation of a conjunction or disjunction requires us to distribute the negation over the formula in the list
convertIntoCNF (Neg (Cnj fs)) = convertIntoCNF (Dsj (map (\x -> (Neg x)) fs))
convertIntoCNF (Neg (Dsj fs)) = convertIntoCNF (Cnj (map (\x -> (Neg x)) fs))

-- negation of Implications and Equivalences require these to be converted first
convertIntoCNF (Neg (Impl f1 f2)) = convertIntoCNF (Neg (convertIntoCNF (Impl f1 f2)))
convertIntoCNF (Neg (Equiv f1 f2)) = convertIntoCNF (Neg (convertIntoCNF (Equiv f1 f2)))

-- conversion of an implication is done by converting the equivalent formula: P -> Q = (-P v Q)
convertIntoCNF (Impl f1 f2) = convertIntoCNF (Dsj [(Neg f1), f2])

-- conversion of an equivalence is done by converting the disjunction of the conjunctives of negative and positive formulas
convertIntoCNF (Equiv f1 f2) = convertIntoCNF (Dsj [(Cnj [f1, f2]), (Cnj [(Neg f1), (Neg f2)])])

-- Test function for the CNF converter, it generates a random formula using the random generator in question 4, then checks if this formula is equivalent to whatever is produced by the converter. The test succeeds on average 15 times before failing.
testCNFConverter :: [Int] -> [Int] -> Bool
testCNFConverter atoms connectives =
    let atomsAct = if length atoms /= 0 then take 20 atoms else [1]
        connectivesAct = if length connectives /= 0 then take 20 connectives else [1]
        generatedFormula = formulaGenerator atomsAct connectivesAct
        parsedFormula = convertIntoCNF generatedFormula
    in equiv parsedFormula generatedFormula

-- =================================
-- == 4: Random Formula Generator ==
-- =================================
{- #####%%%%%%%######%%%%%%%%#####%%%%%%
NOTE: This is not my own implementation, this was copied word for word from Martin's work. I only include this in my file because The code for the solution to Question 2 is dependent on it. The solution to question 2 is wholly my own work.
   #####%%%%%%%######%%%%%%%%#####%%%%%% -}
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

--source: https://stackoverflow.com/a/19074708
--splits list into almost even halves, with left side being larger by 1 if uneven
splitHalf :: [a] -> ([a],[a])
splitHalf l = splitAt ((length l + 1) `div` 2) l
