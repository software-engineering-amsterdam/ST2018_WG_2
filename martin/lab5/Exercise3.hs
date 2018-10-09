module Exercise3 (isMinimal, isSudokuSolutionUnique) where
 
import Data.List
import System.Random
import Lecture5
import Data.Char


--time: 3 hours, first solution with counting possible solutions was wrong
--then researching what it takes to be minimal

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


--tests
checkActualMinimalSudokuIsMinimal = and $ map isSudokuSolutionUnique (map stringToSudoku minimalSudokuExamplesString)
checkNonMinimalSudokuIsNotMinimal = and $ map (not . isMinimal) (map grid2sud [example1, example2, example4])


exercise3 = do 
    print "Checking that minimal sudokus are classified minimal:"
    print checkActualMinimalSudokuIsMinimal
    print "Checking that non-minimal sudokus are classified as non minimal:"
    print checkNonMinimalSudokuIsNotMinimal


--converts 81 character long string to sudoku
stringToSudoku :: String -> Sudoku
stringToSudoku s = grid2sud $ stringToGrid s

--converts 81 character long string to grid
stringToGrid :: String -> Grid
stringToGrid s = [(map digitToInt) (take 9 $ drop (9*dp) s) | dp <- [0..8] ]

example6 :: Grid 
example6 = stringToGrid $ (minimalSudokuExamplesString !! 0)

--source: http://staffhome.ecm.uwa.edu.au/~00013890/sudokumin.php 
--source: http://staffhome.ecm.uwa.edu.au/~00013890/sudoku17 
minimalSudokuExamplesString :: [String]
minimalSudokuExamplesString = 
    ["000000010400000000020000000000050407008000300001090000300400200050100000000806000",
    "000000010400000000020000000000050604008000300001090000300400200050100000000807000",
    "000000012000035000000600070700000300000400800100000000000120000080000040050000600",
    "000000012003600000000007000410020000000500300700000600280000040000300500000000000",
    "000000012008030000000000040120500000000004700060000000507000300000620000000100000",
    "000000012040050000000009000070600400000100000000000050000087500601000300200000000",
    "000000012050400000000000030700600400001000000000080000920000800000510700000003000",
    "000000012300000060000040000900000500000001070020000000000350400001400800060000000",
    "000000012400090000000000050070200000600000400000108000018000000000030700502000000",
    "000000012500008000000700000600120000700000450000030000030000800000500700020000000",
    "000000012700060000000000050080200000600000400000109000019000000000030800502000000",
    "000000012800040000000000060090200000700000400000501000015000000000030900602000000",
    "000000012980000000000600000100700080402000000000300600070000300050040000000010000",
    "000000013000030080070000000000206000030000900000010000600500204000400700100000000",
    "000000013000200000000000080000760200008000400010000000200000750600340000000008000",
    "000000013000500070000802000000400900107000000000000200890000050040000600000010000",
    "000000013000700060000508000000400800106000000000000200740000050020000400000010000",
    "000000013000700060000509000000400900106000000000000200740000050080000400000010000",
    "000000013000800070000502000000400900107000000000000200890000050040000600000010000",
    "000000013020500000000000000103000070000802000004000000000340500670000200000010000",
    "000000013040000080200060000609000400000800000000300000030100500000040706000000000",
    "000000013040000080200060000906000400000800000000300000030100500000040706000000000",
    "000000013040000090200070000607000400000300000000900000030100500000060807000000000",
    "000000013040000090200070000706000400000300000000900000030100500000060807000000000",
    "000000013200800000300000070000200600001000000040000000000401500680000200000070000",
    "000000013400200000600000000000460500010000007200500000000031000000000420080000000",
    "000000013400800000200000070000400900001000000060000000000501600380000200000070000",
    "000000014000000203800050000000207000031000000000000650600000700000140000000300000",
    "000000014000020000500000000010804000700000500000100000000050730004200000030000600",
    "000000014000708000000000000104005000000200830600000000500040000030000700000090001"]


