module Lab5Ex4 where
 
import Data.List
import System.Random
import Lecture5
import Data.Char
import System.Random

-- ============================
-- == Exercise 4 == 4 hours
-- ============================

--determines if a given sudoku is minimal
isMinimal :: Sudoku -> Bool
isMinimal sudoku = (isSudokuSolutionUnique sudoku) && --to satisfy minimality, original must have 1 solution
    (and $ map (not . isSudokuSolutionUnique) -- check if all removed hints produce multiple possibilities (non unique)
        (map (\(r,c) -> removeOneHint sudoku (r,c)) (filledPositions sudoku))) --from all filled positions, remove 1 hint

--checks if given sudoku has only one possible solution
isSudokuSolutionUnique :: Sudoku -> Bool
isSudokuSolutionUnique sudoku = (consistent sudoku) -- inconsistent sudoku produces empty Node list
    && (and $ map uniqueSol (initNode (sud2grid sudoku))) --check uniqueness of input sudoku

--removes one filled position in the sudoku
removeOneHint :: Sudoku -> (Int,Int) -> Sudoku
removeOneHint sudoku (r,c) = update sudoku ((r,c),0)

--generate a sudoku with 'removeNum' removed blocks which is still uniquely solvable
--returns an exceptions if no solution is found in 'maxAttempts' times
generateUniqueUnsolvedSudoku :: Int -> Int -> IO Sudoku
generateUniqueUnsolvedSudoku _ 0 = error "Can not create sudoku with given constraints"
generateUniqueUnsolvedSudoku removeNum maxAttempts  = do
    sudoku <- generateUnsolvedSudoku removeNum
    (minSudoku, _) <- genProblem (head $ initNode (sud2grid sudoku))
    if isSudokuSolutionUnique minSudoku 
        then return minSudoku 
        else generateUniqueUnsolvedSudoku removeNum (maxAttempts - 1 )

--generates a sudoku with 'removeNum' blocks removed
--no guarantee on there being a unique solution
generateUnsolvedSudoku :: Int -> IO Sudoku
generateUnsolvedSudoku removeNum = do
    grids <- generateRandomDistinctGrids removeNum
    (sudoku, _) <- genRandomSudoku
    let unwrappedGrids = nub $ concat $ map gridToCoords grids
    let updateList = map (\(r,c) -> updateFlip ((r,c), 0)) unwrappedGrids 
        updatedSudoku = applyFunctions updateList sudoku
        in
            return (updatedSudoku)

--changed update function to support partial application of sudoku
updateFlip :: ((Row,Column),Value) -> Sudoku -> Sudoku
updateFlip ((r,c), val) sudoku = update sudoku ((r,c),val)

-- >> https://stackoverflow.com/questions/28400825/applying-a-list-of-functions-in-haskell/28400942
-- >> https://stackoverflow.com/questions/47157748/list-of-functions-applying-to-argument 
applyFunctions :: [(a -> a)] -> a -> a
applyFunctions functions input = foldl (\acc fun -> fun acc) input functions

--converts grid index [0,2] to list of all blocks in given grid in range [1,9]
gridToCoords :: (Int, Int)  -> [(Row, Column)]
gridToCoords (inR, inC) = [(r + inR * 3 + 1 , c + inC * 3 + 1) | r <- [0..2], c <- [0..2]]

--generates 'num' distinct grid coordinates in the range [0,2]
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

{-  Findings report:
The function sud3emptyGen generates sudokus with three empty blocks. These are minimized, but the minimization really only makes sense if the three blocks that are randomly chosen aren't in line with each other, i.e, not in the same row or column. This is because if they do align, the problem becomes ambiguous. This is immediately an answer to whether we can build puzzles with more than 3 emtpy blocks: no. If we take 4 empty blocks in the sudoku, at least two of them occur in the same row or column, causing them to be ambiguous and invalid sudokus.
The provided implementation generates random sudokus with randomly chosen blocks to wipe, which causes it to sometimes provide valid sudokus and sometimes not. Below is an example of what a valid puzzle with three empty blocks generated by this function.

If we have two empty block in one row or in on block - column a sudoku is ambiguous. For reasons of complexity, the explanaition is based on two empty blocks in one block - row.

Let the sudoku be solved. Remove 2 blocks in one block - row. Then we have the following constraints:
The only filled-in block in the block - row sets 1 constraint per number, namely that the number is not allowed to be in the same row / line. 
Per empty block there are 2 blocks in the same column. Therefore we have 2 constraints per number, namely that the number is allowed to be only in one row of the empty block. This is because the 2 filled-in blocks already satisfy the sudoku constraints (numbers are not allowed to be more than once in the same column).
If we imagine the block, the blocks in the row forbids to put a number in the two left columns. The block of the block-row forbids to put the number in one row. Therefore 2 free fields are left, where the number can be entered.
The same applies to the second empty block. Since 2 free fields of each the 2 empty blocks are in the same rows, the numbers can be set in 2 ways. They can be switched and the sudoko constraints are still fullfilled. 

+-------+-------+-------+
| 5 3 4 | 6 7 8 | 9 1 2 |
| 6 7 2 | 1 9 5 | 3 4 8 |
| 1 9 8 | 3 4 2 | 5 6 7 |
+-------+-------+-------+
| 8 5 9 | 7 6 1 | 4 2 3 |
| 4 2 6 | 8 5 3 | 7 9 1 |
| 7 1 3 | 9 2 4 | 8 5 6 |
+-------+-------+-------+
| 2'- - | 5 3 7 | 2 -  -|
| 2 - - | 4 1 9 | 2'-  -|
| - - - | 2 8 6 | - -  -|  example for number 2: 2 possible solutions (2 or 2'). Thererfore ambiguous
+-------+-------+-------+

-}

{-  Result:
*Lab5Ex4> generateUniqueUnsolvedSudoku 3 100 >>= showSudoku 
+-------+-------+-------+
| 6     |   3 4 |   9 7 |
|   4   |   6   |   2   |
| 1   9 |     7 | 5 6   |
+-------+-------+-------+
|       | 4     | 9     |
|       |   8 9 | 6     |
|       | 3   6 | 8 1   |
+-------+-------+-------+
| 3   5 |       |       |
|   2   |       |       |
|   7 1 |       |       |
+-------+-------+-------+
-}