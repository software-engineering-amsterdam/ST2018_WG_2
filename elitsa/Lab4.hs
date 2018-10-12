module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck 
import SetOrd
import System.IO.Unsafe



-- probs :: Int -> IO Int
-- probs 0 = return 0
-- probs n = do
--              p <- getStdRandom random
--              return p

-- randomRIO (1,100) returns randomInt

getRandomInt :: Int -> Int
getRandomInt max = unsafePerformIO $ getStdRandom (randomR (0,max))

randomTgen':: Set Int -> Int -> Set Int 
randomTgen' set 0 = 0
randomTgen' set length = randTgen'(((insertSet (getRandomInt 100) set) (length-1)))	

randomTgen:: Int -> Set Int
randomTgen x = randomTgen' emptySet x




-- 6 пъти трябва това да го направя 