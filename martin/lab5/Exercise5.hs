module Exercise5 where
 
import Data.List
import System.Random
import Data.Char
import System.Random
import Exercise3 --helper functions
import Exercise1 --modified lecture5 with NCR


generateUniqueUnsolvedSudoku _ 0 = error "Can not create sudoku with given constraints"
generateUniqueUnsolvedSudoku removeNum maxAttempts  = do
    sudoku <- generateUnsolvedSudoku removeNum
    if isSudokuSolutionUnique sudoku 
        then return sudoku 
        else generateUniqueUnsolvedSudoku removeNum (maxAttempts - 1 )


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
