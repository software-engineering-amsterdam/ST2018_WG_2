module Lab6 where
import Data.List


-- ================= Ex 1 ==============

-- Input x^y mod modulo
exM :: Integer -> Integer -> Integer -> Integer
exM _ 0 _ = 1
exM x 1 base = mod x base
exM x y base = if even y 
               then mod (helperF x y base) base  --even
               else mod ( x * (mod (helperF x (y-1) base) base)) base --odd


helperF:: Integer->Integer->Integer->Integer
helperF _ 0 _  = 1
--helperF x 1 base = mod x base
helperF x y base = (exM x (div y 2) base) * (exM x (div y 2) base)