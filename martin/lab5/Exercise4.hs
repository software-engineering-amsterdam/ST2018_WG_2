module Lab5Ex4 where
 
import Data.List
import System.Random
import Lecture5
import Data.Char
import System.Random
import Exercise3


generateUniqueUnsolvedSudoku _ 0 = error "Can not create sudoku with given constraints"
generateUniqueUnsolvedSudoku removeNum maxAttempts  = do
    sudoku <- generateUnsolvedSudoku removeNum
    if isSudokuSolutionUnique sudoku 
        then return sudoku 
        else generateUniqueUnsolvedSudoku removeNum (maxAttempts - 1 )


generateUnsolvedSudoku removeNum = do
    grids <- generateRandomDistinctGrids removeNum
    (sudoku, _) <- genRandomSudoku
    let unwrappedGrids = nub $ concat $ map gridToCoords grids
    let updateList = map (\(r,c) -> updateFlip ((r,c), 0)) unwrappedGrids 
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


gridToCoords :: (Int, Int)  -> [(Row, Column)]
gridToCoords (inR, inC) = [(r + inR * 3 + 1 , c + inC * 3 + 1) | r <- [0..2], c <- [0..2]]

generateRandomDistinctGrids :: Int -> IO [(Int, Int)] 
generateRandomDistinctGrids num = randUniqueGrid num []

--source: https://stackoverflow.com/questions/27727980/random-numbers-without-duplicates
randUniqueGrid :: Int -> [(Int, Int)] -> IO [(Int, Int)] 
randUniqueGrid num list
    | ((length list) >= num) = return list
    | otherwise = do
        x <- randomRIO (0,2)
        y <- randomRIO (0,2)
        if elem (x,y) list
            then randUniqueGrid num list
            else randUniqueGrid num ((x,y):list)    