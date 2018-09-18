import Data.List
import Numeric
import Data.Char



-- Task 2 

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

isEquilateral:: Int -> Int -> Int -> Bool
isEquilateral a b c = 
    let firstCheck = (a == b)
        secondCheck = (a == c)
    in firstCheck && secondCheck

isRectangular:: Int -> Int -> Int -> Bool
isRectangular a b c = a^2 + b^2 == c^2

isTriangle:: Int -> Int -> Int -> Bool
isTriangle a b c = 
    let firstCheck = (a + b) > c
        secondCheck = (a + c) > b
        thirdCheck = b + c > a
	in firstCheck && secondCheck && thirdCheck

isIscosceles:: Int -> Int -> Int -> Bool
isIscosceles a b c = 
	let firstCheck = (a == b)
	    secondCheck = (a == c) 
	    thirdCheck = (b == c)
	in isEquilateral a b c == False && (firstCheck || secondCheck || thirdCheck)


triangle:: Int -> Int -> Int -> Shape
triangle a b c = 
		if (isTriangle a b c == False) then NoTriangle
		else if (isEquilateral a b c) then Equilateral
		else if (isIscosceles a b c) then Rectangular
		else if (isIscosceles a b c) then Isosceles
		else Other

-- Task 4 

perms :: [a] ->[[a]]
perms [] = [[]] 
perms (x:xs) = 
    concat (map (insrt x) (perms xs)) where 
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

isPermutation:: [Int] -> [Int] ->  Bool  -- first parametr gets the list we are checking if it is permutation / secon parameter is the list that a permutation should be constructed
isPermutation x y = elem x (perms y)

-- TODO: Answer the questions && test


-- Task 6 - Implementing and testing ROT13 encoding

alphabet = ['a'.. 'z']

getIndex:: (Eq a) => a -> [a] -> Int
getIndex l list = getIndex' l list 0 

getIndex'::(Eq a) => a -> [a] -> Int -> Int
getIndex' elem (x:xs) startingPoint = 
                      if elem == x then startingPoint
                      else getIndex' elem xs (startingPoint + 1) 

replaceChar:: Int -> Char 
replaceChar x = 
			 let characterIfMoreThan13 = 13 - (26 - x)
			 in  if (characterIfMoreThan13 >= 0 && x >= characterIfMoreThan13)  then alphabet !! characterIfMoreThan13
                 else alphabet !! (x + 13)

rot13:: [Char] -> [Char]
rot13 [] = []
rot13 list = replaceChar(getIndex(head list) alphabet) : rot13(tail(list)) 
			
			
-- Euler 
-- TASK 1
divisibleBy3or5:: Int -> Int
divisibleBy3or5  boundary = sum [x | x <- [1.. boundary], (mod x 5 == 0 || mod x 3 == 0) && (x < boundary)]

-- Task 2

fibonacci:: Int
fibonacci = sum (filter (even) (fibonacci' 0 1))

fibonacci':: Int -> Int -> [Int]
fibonacci' x y = 
              let numberToAppenad = x + y
              in if numberToAppenad < 4000000 -- restriction provided in the task 
                 then numberToAppenad : fibonacci' y numberToAppenad  
              else []




			