module PascalsTriangle where 

-- input: N where N represents the number of rows 
-- program prints out all rows up to the Nth row of Pascal's Triangle on seperate lines

import Data.List

factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial (n-1)

binomial :: Int -> Int -> Int
binomial n r 
    | n < r = error "column number excedes row number"
    | otherwise = (factorial n) `div` ((factorial r) * factorial (n - r))

-- returns a specific row of Pascal's triangle
-- idea:
-- pascalRow 0 = binomial 0 1 
-- pascalRow 1 = binomial 1 0 ++ binomial 1  1 
-- pascalRow 2 = binomial 2 0 ++ binomial 2 1 ++ binomial 2 2 
pascalRow :: Int -> [Int]
pascalRow n = [binomial n r | r <- rs] 
    where rs = reverse [0..n]

-- creates a list that contains all rows of Pascal's Triangle up to the Nth row
pascalsTriangle :: Int -> [[Int]]
pascalsTriangle 0 = [pascalRow 0]
pascalsTriangle n = pascalsTriangle (n-1) ++ [pascalRow n]

-- takes a list of numbers and concatenates the numbers 
-- in order to create a line of numbers separated by white space
stringRow :: [Int] -> String 
stringRow [] = ""
stringRow (n:ns) = (show n) ++ " " ++ stringRow ns 

-- run `main` in ghci order to execute the program
main :: IO ()
main = do
  putStrLn "############ Pascals Triangle ############"
  putStrLn "Please enter the number of rows:"
  x <- getLine
  let pTriangle = pascalsTriangle ((read x) - 1)
  
  do 
    --mapM_ (print . stringRow) pTriangle
    putStrLn ""
    putStrLn . intercalate "\n" .  map (stringRow) $ pTriangle
