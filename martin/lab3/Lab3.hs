module Lab3 where
 
import Data.List
import System.Random
import Test.QuickCheck
import Data.Char
--import Lecture3

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

--- ##### ex1 : 2 hours

contradiction :: Form -> Bool
contradiction formula = forall (allVals formula) (\val -> not $ (evl val formula))

tautology :: Form -> Bool
tautology formula = forall (allVals formula) (\val -> (evl val formula))


-- logical entailment  A |= B  direction (slides are reversed!)
entails :: Form -> Form -> Bool
entails formulaA formulaB = forall (filter (\value -> evl value formulaB) (allVals formulaB)) (\value -> evl value formulaA )

--TODO: select one implementation, see direction A |= B vs A =| B ??
entails' formulaA formulaB = and $ zipWith (\b a -> evl b formulaB --> evl a formulaA ) (allVals formulaB) (allVals formulaA)


--determines logical equivalence of 2 formulas
equiv:: Form -> Form -> Bool
equiv formulaA formulaB = tautology $ Equiv formulaA formulaB



--ex 1 manual test cases: 

exercise1TestCases :: [(Bool, Bool)]
exercise1TestCases = [
        --(function args -> result, expected)
        (equiv (Neg (Prop 1)) (Neg (Prop 1)), True),
        (equiv form1 form1, True),
        (equiv form1 form2, False),
        (equiv form2 form2, True),
        (equiv form3 form3, True)
        --------------------------
        --TODO: manually verify
        --(contradiction form1, ),
        --(contradiction form2, ),
        --(contradiction form3, ),
        --------------------------
        --------------------------
        --TODO: manually verify
        --(tautology form1, ),
        --(tautology form2, ),
        --(tautology form3, ),
        --------------------------
        --TODO: entails test cases

    ]

exercise1ManualTestCaseVerifier :: [Bool]
exercise1ManualTestCaseVerifier = map (\(actual, expected) -> actual == expected) exercise1TestCases


parseString :: String -> Form
parseString x = head $ parse x

--ex 2:

--TODO!!!!

-- ex 3

--in this Haskell implementation '*()' is always true, but is not valid formula
cnfTautologyStatement = (Dsj [Neg (Prop 1), Prop 1])


--converts a formula to CNF formula
cnf :: Form -> Form
cnf formula 
    | tautology formula = cnfTautologyStatement
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


--exercise 3 manual tests: 
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


-- ex 4

--data type with all possible symbols to be found in a logical formula
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



--time: 2:25h


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

--https://github.com/commercialhaskell/stack/issues/394
--https://github.com/atom-haskell/ide-haskell/issues/152
 

 -- bonus @ 4 hours
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



-- ======================================
-- ======================================
-- ======================================
-- ======================================    

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

infixl 2 #

(#) :: (a -> b) -> (b -> c) -> (a -> c)
(#) = flip (.)

infixl 1 $$

($$) :: a -> (a -> b) -> b
($$) = flip ($)

update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (x,y) = \ z -> if x == z then y else f z 

updates :: Eq a => (a -> b) -> [(a,b)] -> a -> b
updates = foldl update 

type Var = String
type Env = Var -> Integer

data Expr = I Integer
          | V Var 
          | Add Expr Expr 
          | Subtr Expr Expr 
          | Mult Expr Expr 
          deriving (Eq,Show)

eval :: Expr -> Env -> Integer 
eval (I i) _ = i 
eval (V name) env = env name
eval (Add e1 e2) env = (eval e1 env) + (eval e2 env)
eval (Subtr e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Mult e1 e2) env = (eval e1 env) * (eval e2 env)

assign :: Var -> Expr -> Env -> Env 
assign var expr env =  update env (var, eval expr env)

initEnv :: Env 
initEnv = \ _ -> undefined

initE :: Env
initE = const undefined

example = initEnv $$ 
          assign "x" (I 3) # 
          assign "y" (I 5) # 
          assign "x" (Mult (V "x") (V "y")) #
          eval (V "x")

while :: (a -> Bool) -> (a -> a) -> a -> a
while = until . (not.)

euclid m n = (m,n) $$
   while (\ (x,y) -> x /= y) 
         (\ (x,y) -> if x > y then (x-y,y) 
                              else (x,y-x)) #
         fst

euclid' m n = fst $ eucl (m,n) where
     eucl = until (uncurry  (==))
         (\ (x,y) -> if x > y then (x-y,y) else (x,y-x))

whiler :: (a -> Bool) -> (a -> a) -> (a -> b) -> a -> b
whiler p f r = while p f # r

euclid2 m n = (m,n) $$
          whiler (\ (x,y) -> x /= y) 
                 (\ (x,y) -> if x > y then (x-y,y) 
                                      else (x,y-x))
                 fst

fibonacci :: Integer -> Integer
fibonacci n = fibon (0,1,n) where
  fibon = whiler 
           (\ (_,_,n) -> n > 0)
           (\ (x,y,n) -> (y,x+y,n-1))
           (\ (x,_,_) -> x)

fb :: Integer -> Integer
fb n = fb' 0 1 n where 
   fb' x y 0 = x 
   fb' x y n = fb' y (x+y) (n-1)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

type Name = Int

data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form 
          | Equiv Form Form 
          deriving (Eq,Ord)

instance Show Form where 
  show (Prop x)   = show x
  show (Neg f)    = '-' : show f 
  show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
  show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>" 
                           ++ show f2 ++ ")"
  show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>" 
                           ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

p = Prop 1
q = Prop 2
r = Prop 3 

form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)

propNames :: Form -> [Name]
propNames = sort.nub.pnames where 
  pnames (Prop name) = [name]
  pnames (Neg f)  = pnames f
  pnames (Cnj fs) = concatMap pnames fs
  pnames (Dsj fs) = concatMap pnames fs
  pnames (Impl f1 f2)  = concatMap pnames [f1,f2]
  pnames (Equiv f1 f2) = concatMap pnames [f1,f2]

type Valuation = [(Name,Bool)]

-- | all possible valuations for lists of prop letters
genVals :: [Name] -> [Valuation]
genVals [] = [[]]
genVals (name:names) = 
  map ((name,True) :) (genVals names)
  ++ map ((name,False):) (genVals names)

-- | generate all possible valuations for a formula
allVals :: Form -> [Valuation]
allVals = genVals . propNames

type ValFct = Name -> Bool

val2fct :: Valuation -> ValFct
val2fct = updates (\ _ -> undefined)

fct2val :: [Name] -> ValFct -> Valuation
fct2val domain f = map (\x -> (x,f x)) domain 

evl :: Valuation -> Form -> Bool
evl [] (Prop c)    = error ("[mismatched variable count or names] no info: " ++ show c)
evl ((i,b):xs) (Prop c)
     | c == i    = b
     | otherwise = evl xs (Prop c)
evl xs (Neg f)  = not (evl xs f)
evl xs (Cnj fs) = all (evl xs) fs
evl xs (Dsj fs) = any (evl xs) fs
evl xs (Impl f1 f2) = evl xs f1 --> evl xs f2
evl xs (Equiv f1 f2) = evl xs f1 == evl xs f2

satisfiable :: Form -> Bool
satisfiable f = any (\ v -> evl v f) (allVals f)

data Token 
      = TokenNeg
      | TokenCnj
      | TokenDsj
      | TokenImpl
      | TokenEquiv 
      | TokenInt Int 
      | TokenOP
      | TokenCP
 deriving (Show,Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lexNum (c:cs) 
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('*':cs) = TokenCnj : lexer cs
lexer ('+':cs) = TokenDsj : lexer cs
lexer ('-':cs) = TokenNeg : lexer cs 
lexer ('=':'=':'>':cs) = TokenImpl : lexer cs
lexer ('<':'=':'>':cs) = TokenEquiv : lexer cs
lexer (x:_) = error ("unknown token: " ++ [x])

lexNum cs = TokenInt (read num) : lexer rest
     where (num,rest) = span isDigit cs

type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed x xs = [(x,xs)]

parseForm :: Parser Token Form 
parseForm (TokenInt x: tokens) = [(Prop x,tokens)]
parseForm (TokenNeg : tokens) =
  [ (Neg f, rest) | (f,rest) <- parseForm tokens ]
parseForm (TokenCnj : TokenOP : tokens) = 
  [ (Cnj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenDsj : TokenOP : tokens) = 
  [ (Dsj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenOP : tokens) = 
  [ (Impl f1 f2, rest) | (f1,ys) <- parseForm tokens,
                         (f2,rest) <- parseImpl ys ]
   ++
  [ (Equiv f1 f2, rest) | (f1,ys) <- parseForm tokens,
                          (f2,rest) <- parseEquiv ys ] 
parseForm tokens = []

parseForms :: Parser Token [Form] 
parseForms (TokenCP : tokens) = succeed [] tokens
parseForms tokens = 
   [(f:fs, rest) | (f,ys) <- parseForm tokens, 
                   (fs,rest) <- parseForms ys ]

parseImpl :: Parser Token Form
parseImpl (TokenImpl : tokens) = 
  [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseImpl tokens = []

parseEquiv :: Parser Token Form
parseEquiv (TokenEquiv : tokens) = 
  [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseEquiv tokens = []

parse :: String -> [Form]
parse s = [ f | (f,_) <- parseForm (lexer s) ]

arrowfree :: Form -> Form 
arrowfree (Prop x) = Prop x 
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) = 
  Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) = 
  Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
  where f1' = arrowfree f1
        f2' = arrowfree f2

nnf :: Form -> Form 
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)

