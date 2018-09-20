module Lab3 

where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- =========================
-- == 1: Formula Analysis == 35 mins
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


-- ============================
-- == 2: Parse Function Test ==
-- ============================



-- ====================================
-- == 3: Conjunctive Normal Function ==
-- ====================================

t3_1 = p
t3_2 = (Neg p)
t3_3 = (Neg (Neg p))
t3_4 = (Cnj [p,q])

isConjNormFormTest = (Cnj [Dsj [(Neg p), (Equiv p q)], Dsj [r, p, (Neg p)], Dsj [r, (Neg q)]])

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

-- conversion of an implication is done by converting the equivalent formula: P -> Q = (-P v Q)
convertIntoCNF (Impl f1 f2) = convertIntoCNF (Dsj [(Neg f1), f2])

-- conversion of an equivalence is done by converting the disjunction of the conjunctives of negative and positive formulas
convertIntoCNF (Equiv f1 f2) = convertIntoCNF (Dsj [(Cnj [f1, f2]), (Cnj [(Neg f1), (Neg f2)])])

