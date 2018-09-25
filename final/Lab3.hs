module Lab3 where
 
import Data.List
import System.Random
import Test.QuickCheck
import Data.Char
import Lecture3

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- ============================
-- == 1: Logical properties == 4:30 hours
-- ============================

-- helper function that evaluates all valuations for a given formula by applying 
-- them to said f. The resulting list is then a truth-table for this formula
solutions :: Form -> [Bool]
solutions f = map (\x -> evl x f) (allVals f)

-- the formula is a contradiction if the truth table contains no True Bools
contradiction :: Form -> Bool
contradiction f = not $ or $ solutions f

-- the formula is a contradiction if the truth table contains only True Bools
tautology :: Form -> Bool
tautology f = and $ solutions f


-- logical entailment  A |= B  direction
-- f1 entails f2 if the truth table of f1 entails the truth table of f2
entails :: Form -> Form -> Bool
entails f1 f2 = and $ zipWith (-->) (solutions f1) (solutions f2)

--determines logical equivalence of 2 formulas
equiv:: Form -> Form -> Bool
equiv formulaA formulaB = tautology $ Equiv formulaA formulaB

-- %%%%%
-- %%  Testing
-- %%%%%
contradictionExample = Cnj [(Prop 1), Neg(Prop 1)]

exercise1TestCases :: [(Bool, Bool)]
exercise1TestCases = [
        --(function args -> result, expected)
        --------------------------
        (equiv (Neg (Prop 1)) (Neg (Prop 1)), True),
        (equiv form1 form1, True),
        (equiv form1 form2, False),
        (equiv form2 form2, True),
        (equiv form3 form3, True),
        --------------------------
        (contradiction form1, False),
        (contradiction form2, False),
        (contradiction form3, False),
        (contradiction cnfTautologyStatement, False),
        (contradiction contradictionExample, True),
        --------------------------
        (tautology form1, True),
        (tautology form2, False),
        (tautology form3, True),
        (tautology cnfTautologyStatement, True),
        --------------------------
        (entails form1 form2, False),
        (entails form2 form1, True),
        (entails contradictionExample cnfTautologyStatement, True),
        (entails contradictionExample cnfTautologyStatement, True)
        --------------------------
    ]

exercise1ManualTestCaseVerifier :: [Bool]
exercise1ManualTestCaseVerifier = map (\(actual, expected) -> actual == expected) exercise1TestCases

exercise1TestResults = do 
    print "Testing the functions checking formula types..."
    print "Checking if (A ^ not(A)) is a contradiction..."
    print (contradiction (Cnj [Prop 1, (Neg (Prop 1))]))
    print "Checking if (A v not(A)) is a tautology..."
    print (tautology (Dsj [Prop 1, (Neg (Prop 1))]))
    print "Checking if (A ^ B) entails A..."
    print (entails (Cnj [(Prop 1), (Prop 2)]) (Prop 1))
    print "Checking if (A -> B) is equivalent to not(A) or B..."
    print (equiv (Impl (Prop 1) (Prop 2)) (Dsj [(Neg (Prop 1)), (Prop 2)]))
    print "Checking other manual test cases..."
    print (and exercise1ManualTestCaseVerifier)
    print "If all of the above returned True, the test was succesful"

{- Test Results:
*Lab3> exercise1TestResults 
"Testing the functions checking formula types..."
"Checking if (A ^ not(A)) is a contradiction..."
True
"Checking if (A v not(A)) is a tautology..."
True
"Checking if (A ^ B) entails A..."
True
"Checking if (A -> B) is equivalent to not(A) or B..."
True
"Checking other manual test cases..."
True
"If all of the above returned True, the test was succesful"
-}


-- ============================
-- == 2: Parse Function Test == 2:30 hours
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

--in this Haskell implementation '*()' is always true, but is not valid formula
cnfTautologyStatement = (Dsj [Neg (Prop 1), Prop 1])

--converts a formula to CNF formula
cnf :: Form -> Form
cnf formula 
    | tautology formula = cnfTautologyStatement -- by default only "*()" is generated as CNF statement for tautologies, but this is not a valid formula
    | otherwise = Cnj mappedLines
    where 
        falseVals = filter (\val -> not $ evl val formula) (allVals formula) --only get NON-satisfied evaluations
        mappedLines = map cnfLine falseVals --map each satisfied evaluation to list of clauses, with outer conjunction

--converts one valuation to CNF clause
cnfLine :: Valuation -> Form
cnfLine vx = Dsj (map cnfVar vx)

--converts variables according to CNF conversion rules to A or NOT A
cnfVar :: (Name,Bool) -> Form
cnfVar (name, False) = Prop name
cnfVar (name, True) = Neg (Prop name)

-- %%%%%
-- %%  Testing
-- %%%%%
exercise3ManualTestCases = [
        --(original formula, expected result)
        (Neg (Prop 1), Neg (Prop 1)),
        (Prop 1, Prop 1),
        (Cnj[Neg(Prop 1), (Prop 1)], Cnj [Dsj [Neg (Prop 1)], Dsj [Prop 1]]), --contradiction
        (Dsj[Neg(Prop 1), (Prop 1)], cnfTautologyStatement) --tautology that is valid formula
    ]

exercise3ManualTestCaseVerifier = map (\(original, expected) -> (let converted = cnf original in (equiv expected converted) && isConjNormForm converted)) exercise3ManualTestCases

exercise3ManualTestResults = do 
    print "Testing manual test cases"
    print (exercise3ManualTestCaseVerifier)
    print "Exercise 3 automated QuickCheck tests are in exercise 4!"

{-
Test results:
*Lab3> exercise3ManualTestResults 
"Testing manual test cases"
[True,True,True,True]
-}

-- Test function for the CNF converter, it generates a random formula using the random generator in question 4, then checks if this formula is equivalent to whatever is produced by the converter. The output of this test case is included in the results in exercise 4.
testCNFConverter :: [Int] -> [Int] -> Bool
testCNFConverter atoms connectives =
    let atomsAct = if length atoms /= 0 then take 10 atoms else [1]
        connectivesAct = if length connectives /= 0 then take 10 connectives else [1]
        generatedFormula = formulaGenerator atomsAct connectivesAct
        parsedFormula = cnf generatedFormula
    in equiv parsedFormula generatedFormula


-- =================================
-- == 4: Random Formula Generator ==
-- == time: 5:30 hours
-- =================================

--data type with all possible symbols to be found in a logical formula
data FormGenConnectives 
    = Literal | Not | And | Or | Implies | Equivalent 
    deriving (Eq,Ord, Enum, Show)


--source: https://stackoverflow.com/a/25924694
generateEnumValuesGeneric  :: (Enum a) => [a]
generateEnumValuesGeneric  = enumFrom (toEnum 0)

connectivesList :: [FormGenConnectives]
connectivesList = generateEnumValuesGeneric


--deterministically generates a formula given a list of atoms and connectives as int
formulaGenerator :: [Int] -> [Int] -> Form
formulaGenerator literalsNum connectivesNum = 
    let literalsNumAbs = map abs literalsNum
        connectivesNumMod = map (\x -> (abs x) `mod` (length connectivesList)) connectivesNum
        connectivesEnum = map toEnum  connectivesNumMod
    in formulaGenerator' literalsNumAbs connectivesEnum


--deterministically generates a formula given a list of atoms and connectives as enum
--base cases: next step has no more literals to output OR connectives are empty
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

--splits literals and connectives into almost even halves
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


--testing properties for CNF

--determines if formula satisfies the syntax rules of conjunctive normal form
isConjNormForm :: Form -> Bool
isConjNormForm (Cnj listOfClauses) = and $ map isClause listOfClauses
isConjNormForm f = isClause f || isLiteral f

--determines if formula is a valid CNF clause
isClause :: Form -> Bool
isClause (Dsj listOfLiterals) = and $ map isLiteral listOfLiterals
isClause f = isLiteral f

--determines if a formula is a valid CNF literal ({A, !A} only)
isLiteral :: Form -> Bool
isLiteral (Prop _) = True
isLiteral (Neg (Prop _)) = True
isLiteral _ = False


--property to determine if formula contains any implications or equivalences
isArrowFreeProperty :: Form -> Bool
isArrowFreeProperty (Prop _) = True
isArrowFreeProperty (Neg(formula)) = isArrowFreeProperty formula
isArrowFreeProperty (Cnj form) = and $ map isArrowFreeProperty form
isArrowFreeProperty (Dsj form) = and $ map isArrowFreeProperty form
isArrowFreeProperty (Impl _ _) = False
isArrowFreeProperty (Equiv _ _) = False

--quickcheck tests for exercise 3 and 4
automatedTestEx34 = do
    print "Testing equivalance of QuickCheck generated formula and converted to CNF formula"
    quickCheckResult(\literalsNum connectivesNum -> 
        let formula = formulaGenerator (take 10 literalsNum) (take 10 connectivesNum)
            cnfFormula = cnf formula
        in (not $ null literalsNum) && (not $ null connectivesNum) --> (equiv formula cnfFormula ))
    print "Testing arrowfreeness of converted CNF formula"
    quickCheckResult(\literalsNum connectivesNum -> 
        let formula = formulaGenerator (take 10 literalsNum) (take 10 connectivesNum)
            cnfFormula = cnf formula
        in (not $ null literalsNum) && (not $ null connectivesNum) --> (isArrowFreeProperty cnfFormula ))
    print "Testing format of converted CNF formula being in CNF (no negated clauses, no nesting, ...) "
    quickCheckResult(\literalsNum connectivesNum -> 
        let formula = formulaGenerator (take 10 literalsNum) (take 10 connectivesNum)
            cnfFormula = cnf formula
        in (not $ null literalsNum) && (not $ null connectivesNum) --> (isConjNormForm cnfFormula ))
    print "Testing the Conjunctive Normal Form converter using quickCheck..."
    quickCheckResult(\literalsNum connectivesNum -> testCNFConverter literalsNum connectivesNum)

{-
Test Results:
*Lab3> automatedTestEx34 
"Testing equivalance of QuickCheck generated formula and converted to CNF formula"
+++ OK, passed 100 tests.
"Testing arrowfreeness of converted CNF formula"
+++ OK, passed 100 tests.
"Testing format of converted CNF formula being in CNF (no negated clauses, no nesting, ...) "
+++ OK, passed 100 tests.
"Testing the Conjunctive Normal Form converter using quickCheck..."
+++ OK, passed 100 tests.
Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}
-}


-- =====================
-- == bonus @ 4 hours ==
-- =====================
type Clause  = [Int]
type Clauses = [Clause]

--precondition: is CNF
cnf2cls :: Form -> Clauses
cnf2cls (Dsj formulas) = [(map cnf2cls'' formulas)] --TODO: better way? is this exhaustive?
cnf2cls (Cnj form) = map cnf2cls' form 
--cnf2cls (Neg (Prop name)) = [-name] --TODO: better way?
--cnf2cls (Dsj formulas) =  (map cnf2cls' formulas)

cnf2cls' :: Form -> Clause
--TODO: can be just a ;; -a ??? clarify
cnf2cls' (Prop name) = [name] --TODO: better way?
cnf2cls' (Neg (Prop name)) = [-name] --TODO: better way?
cnf2cls' (Dsj formulas) =  (map cnf2cls'' formulas)


--converts single property (literal) or negated property 
cnf2cls'' :: Form -> Int
cnf2cls'' (Prop name) = name
cnf2cls'' (Neg (Prop name)) = -name

--TODO: tests
