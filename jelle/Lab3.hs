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
    