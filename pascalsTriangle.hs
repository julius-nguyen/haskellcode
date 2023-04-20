module PascalsTriangle where 

import Data.List

factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial (n-1)

binomial :: Int -> Int -> Int
binomial n r 
    | n < r = error "column number excedes row number"
    | otherwise = (factorial n) `div` ((factorial r) * factorial (n - r))

-- returns a specific row of Pascal's triangle
pascalRow :: Int -> [Int]
pascalRow n = [binomial n r | r <- rs] 
    where rs = reverse [0..n]

--pascalRow 0 = binomial 0 1 
--pascalRow 1 = binomial 1 0 ++ binomial 1  1 
--pascalRow 2 = binomial 2 0 ++ binomial 2 1 ++ binomial 2 2 

pascalsTriangle :: Int -> [[Int]]
pascalsTriangle 0 = [pascalRow 0]
pascalsTriangle n = pascalsTriangle (n-1) ++ [pascalRow n]

stringRow :: [Int] -> String 
stringRow [] = ""
stringRow (n:ns) = (show n) ++ " " ++ stringRow ns 


main :: IO ()
main = do
  x <- getLine
  let pTriangle = pascalsTriangle (read x)
  
  do 
    --mapM_ (print . stringRow) pTriangle
    putStrLn ""
    putStrLn . intercalate "\n" .  map (stringRow) $ pTriangle
