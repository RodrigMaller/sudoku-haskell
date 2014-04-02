import System.IO
import System.Environment 
import Control.Monad

--module Sudoku
--(
--  SudokuGrid(...),
--  readFile
--) where

type SudokuGrid = [[Maybe Int]] 

readSudoku :: String -> IO SudokuGrid
readSudoku path = do  
                contents <- readFile path
                let contents' = read contents :: SudokuGrid 
                return contents'

showSudoku :: SudokuGrid -> String
showSudoku grid = foldr (\a acc -> if a == ']' then "]\n" ++ acc else a:acc) "" (show grid)

printSudoku :: SudokuGrid -> IO ()
printSudoku a = putStrLn $ showSudoku a

main = do
     args <- getArgs
     (readSudoku $ (args !! 0)) >>= (\a -> printSudoku a)
