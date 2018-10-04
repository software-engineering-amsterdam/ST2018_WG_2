module Lab5Ex4 where
 
import Data.List
import System.Random
import Lecture5
import Data.Char
import System.Random




generateUnsolvedSudoku removeNum = do
    coords <- generateRandomDistinctCoordinates removeNum
    (sudoku, _) <- genRandomSudoku
    let updateList = map (\(r,c) -> updateFlip ((r,c), 0)) coords 
        updatedSudoku = applyFunctions updateList sudoku
        in
            return (updatedSudoku)

--generateUnsolvedSudoku 5 >>= showSudoku

updateFlip :: ((Row,Column),Value) -> Sudoku -> Sudoku
updateFlip ((r,c), val) sudoku = update sudoku ((r,c),val)

--todo: rewrite with fold?
-- >> https://stackoverflow.com/questions/28400825/applying-a-list-of-functions-in-haskell/28400942
-- >> https://stackoverflow.com/questions/47157748/list-of-functions-applying-to-argument 
applyFunctions :: [(a -> a)] -> a -> a
applyFunctions [] input = input
applyFunctions (fun:functions) input = applyFunctions functions (fun input)


generateRandomDistinctCoordinates :: Int -> IO [(Row,Column)] 
generateRandomDistinctCoordinates num = randUniqueCoord num []

--source: https://stackoverflow.com/questions/27727980/random-numbers-without-duplicates
randUniqueCoord :: Int -> [(Row,Column)] -> IO [(Row,Column)] 
randUniqueCoord num list
    | ((length list) >= num) = return list
    | otherwise = do
        x <- randomRIO (1,9)
        y <- randomRIO (1,9)
        if elem (x,y) list
            then randUniqueCoord num list
            else randUniqueCoord num ((x,y):list)    


generateRandomDistinctGrids :: Int -> IO [Int] 
generateRandomDistinctGrids num = randUniqueGrid num []

--source: https://stackoverflow.com/questions/27727980/random-numbers-without-duplicates
randUniqueGrid :: Int -> [Int] -> IO [Int] 
randUniqueGrid num list
    | ((length list) >= num) = return list
    | otherwise = do
        x <- randomRIO (1,9)
        if elem x list
            then randUniqueGrid num list
            else randUniqueGrid num (x:list)    