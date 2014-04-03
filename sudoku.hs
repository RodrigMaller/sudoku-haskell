import System.IO
import Data.Array.IO
import System.Environment 
import Control.Monad
import Data.Matrix
import Data.List.Split.Internals
import qualified Data.Vector as Vector
import qualified Data.Set as Set
import System.Random

---------- IO -------------------
type Grid       = [[Int]] 
type SudokuGrid = Matrix Int

-- Lê um arquivo em path e preenche o objeto SudokuGrid
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
     solved2Sudoku <- solveByGeneticAlgorithm sudoku 
     printSudoku $ Just solved2Sudoku

----------- PSR ----------------

type Index = (Int,Int) -- (x,y)

-- Retorna uma lista com todos os Indexs (Int,Int) que estão com 0, ou seja, 
-- estão vazios. 
getIndexs :: SudokuGrid -> [Index]
getIndexs grid = [(x,y) | x <- [1..n], y <- [1..m], 0 == (getElem x y grid)]
            where 
              n = nrows grid
              m = ncols grid


-- Retorna True se o número number na posição (x,y) segue todas as 3 regras do 
-- Sudoku, que são: - Diferentes números na linha, - Diferentes número na coluna, 
--                  - Diferentes números nos blocos (3x3) 
-- Caso contrario retorna False.
isValid :: Index -> Int -> SudokuGrid -> Bool
isValid (x,y) number grid = ((Vector.length (Vector.filter (\a -> (a == number)) (getRow x grid))) == 0) && 
                            ((Vector.length (Vector.filter (\a -> (a == number)) (getCol y grid))) == 0) &&
                            isBlockValid (x,y) number grid

isBlockValid :: Index -> Int -> SudokuGrid -> Bool
isBlockValid (x,y) number grid= ((length (filter (\(x',y') -> number == (getElem x' y' grid)) blockIndexs)) == 0)
                     where
                        blockX = ((div (x - 1) 3) * 3) + 1
                        blockY = ((div (y - 1) 3) * 3) + 1
                        blockIndexs = [(x',y') | x' <- [blockX..(blockX+2)], y' <- [blockY..(blockY+2)]]

-- Retorna uma lista com todos os números que são válidos para a posição (x,y)
possibleNumbersToPos :: Index -> SudokuGrid -> ((Int,Int) -> Int -> SudokuGrid -> Bool) -> [Int]
possibleNumbersToPos (x,y) grid filt = [z | z <- [1..9], filt (x,y) z grid]

-- Adiciona o número n no grid na posição idx
mark :: Index -> SudokuGrid -> Int -> SudokuGrid
mark idx grid n = (setElem n idx grid)

-- Retorna uma lista com Sudokus com todos os números válidos aplicados na posição (x,y)
mapMark :: Index -> SudokuGrid -> [SudokuGrid]
mapMark idx grid = map (mark idx grid) (possibleNumbersToPos idx grid isValid)

-- Cria e poda a arvore de recursão
findSolution :: [SudokuGrid] -> [Index] -> Maybe SudokuGrid
findSolution []    _  = Nothing
findSolution (h:t) [] = Just h 
findSolution (h:t) all@((x,y):ls) = let sol = solve ls h in
                                    if sol == Nothing 
                                    then findSolution t all
                                    else sol

solve :: [Index] -> SudokuGrid -> Maybe SudokuGrid
solve []             grid = Just grid
solve idxs@((x,y):_) grid = do
                        guard((length pos) > 0)
                        findSolution pos idxs
              where 
                pos = (mapMark (x,y) grid)
                                                                       
solveSudoku :: SudokuGrid -> Maybe SudokuGrid
solveSudoku grid = solve (getIndexs grid) grid 

--------------- Genetic Algorithm --------------

type Specimen = (Int,SudokuGrid) -- (Cost, DNA)

shuffle :: [Int] -> IO [Int]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

generateASolution :: [Index] -> SudokuGrid -> IO SudokuGrid
generateASolution [] grid      = return grid
generateASolution ((x,y):t) grid = if (getElem x y grid) == 0 
                                   then do 
                                        z <- shuffle (possibleNumbersToPos (x,y) grid isBlockValid)
                                        generateASolution t (setElem (head z) (x,y) grid)
                                   else generateASolution t grid

solutionCost :: SudokuGrid -> Int
solutionCost grid = foldr (\(x,y) acc -> acc + if isValid (x,y) (getElem x y grid) grid 
                                               then 0 
                                               else 1) 
                          0 [(x,y) | x <- [1..(nrows grid)], y <- [1..(ncols grid)]]


--sex :: SudokuGrid -> SudokuGrid -> SudokuGrid

--geneticAlgorithm :: [SudokuGrid] -> [SudokuGrid]

solveByGeneticAlgorithm :: SudokuGrid -> IO SudokuGrid
solveByGeneticAlgorithm a = generateASolution (getIndexs a) a

------------------------------------------------
