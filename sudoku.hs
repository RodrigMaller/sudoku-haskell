import System.IO
import System.Environment 
import Control.Monad
import Data.Matrix
import qualified Data.Vector as Vector

type Grid       = [[Int]] 
type SudokuGrid = Matrix Int

readSudoku :: String -> IO SudokuGrid
readSudoku path = do  
                contents <- readFile path
                let contents' = read contents :: Grid 
                return (fromLists contents')

printSudoku :: Maybe SudokuGrid -> IO ()
printSudoku a = putStrLn $ show a

main = do
     args <- getArgs
     sudoku <- readSudoku $ (args !! 0)
     printSudoku $ Just sudoku
     let solvedSudoku = solveSudoku sudoku
     printSudoku solvedSudoku

----------- PSR ----------------

type Index = (Int,Int)

getIndexs :: SudokuGrid -> [Index]
getIndexs grid = [(x,y) | x <- [1..n], y <- [1..m], 0 == (getElem x y grid)]
            where 
              n = nrows grid
              m = ncols grid

isValid :: Index -> Int -> SudokuGrid -> Bool
isValid (x,y) number grid = ((Vector.length (Vector.filter (\a -> (a == number)) (getRow x grid))) == 0) && 
                            ((Vector.length (Vector.filter (\a -> (a == number)) (getCol y grid))) == 0) &&
                            ((length (filter (\(x',y') -> number == (getElem x' y' grid)) blockIndexs)) == 0)
                     where
                        blockX = ((div (x - 1) 3) * 3) + 1
                        blockY = ((div (y - 1) 3) * 3) + 1
                        blockIndexs = [(x',y') | x' <- [blockX..(blockX+2)], y' <- [blockY..(blockY+2)]]

possibleNumbersToPos :: Index -> SudokuGrid -> [Int]
possibleNumbersToPos (x,y) grid = [z | z <- [1..9], isValid (x,y) z grid]

mark :: Index -> SudokuGrid -> Int -> SudokuGrid
mark idx grid n = (setElem n idx grid)

mapMark :: Index -> SudokuGrid -> [SudokuGrid]
mapMark idx grid = map (mark idx grid) (possibleNumbersToPos idx grid)

findSol :: [SudokuGrid] -> [Index] -> Maybe SudokuGrid
findSol []    _  = Nothing
findSol (h:t) [] = Just h 
findSol (h:t) all@((x,y):ls) = let sol = solve ls h in
                                if sol == Nothing 
                                then findSol t all
                                else sol

solve :: [Index] -> SudokuGrid -> Maybe SudokuGrid
solve []         grid = Just grid
solve idxs@((x,y):_) grid = do
                        guard((length pos) > 0)
                        findSol pos idxs
              where 
                pos = (mapMark (x,y) grid)
                                                                       
solveSudoku :: SudokuGrid -> Maybe SudokuGrid
solveSudoku grid = solve (getIndexs grid) grid 

--------------- Genetic Algorithm --------------

