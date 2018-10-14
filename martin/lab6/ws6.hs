module Workshop6 where
import Data.Char
import Data.List


data Blt a = Leaf a | Node (Blt a) (Blt a) deriving (Eq,Show)

exampleTree :: Blt String
exampleTree = Node (Node (Leaf "Hoare, Tony")
                (Leaf "Turing, Alan"))
                (Leaf "Goedel, Kurt")

leafCount :: Blt a -> Int
leafCount (Leaf _) = 1
leafCount (Node l r) = leafCount l + leafCount r

--proof on paper

mapB :: (a -> b) -> Blt a -> Blt b
mapB fun (Leaf val) = Leaf (fun val)
mapB fun (Node l r) = Node (mapB fun l) (mapB fun r)


fctGcd :: Integer -> Integer -> (Integer,Integer)
fctGcd a b =
 if b == 0
 then (1,0)
 else
 let
 (q,r) = quotRem a b
 (s,t) = fctGcd b r -- gcd(b,r)= b*s + r*t =b*s + (a-b*q)*t = a*t + b*(s-q*t)
 in (t, s - q*t) 



data Tree a = T a [Tree a] deriving (Eq,Ord,Show)
example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1,example1,example1]

count :: Tree a -> Int
count (T _ []) = 1
count (T _ list) = sum (map count list) + 1
