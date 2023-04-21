import Control.Monad

polygon1 :: [(Int,Int)]
polygon1 = [(0,0),(0,1),(1,1),(1,0)]

flatten :: [[a]] -> [a]
flatten [] = []
flatten (xs:xxs) = xs ++ flatten xxs

coord :: [[Int]] -> [(Int,Int)]
coord [] = []
coord [[]] = []
coord (xs:xxs) = (x,xx) : coord xxs 
  where (x:xx:[]) = xs

intlist :: [[String]] -> [[Int]]
intlist [] = []
intlist (xs:xxs) = map read xs : intlist xxs

pythagoras :: (Int,Int) -> (Int,Int) -> Float
pythagoras (x1,y1) (x2,y2) = sqrt (fromIntegral ((x1-x2)^2 + (y1-y2)^2))

-- places the first element at the end of the list
revFirst :: [a] -> [a]
revFirst xs = tail xs ++ [head xs]

-- coordinates for both ends of the sides 
sidesCoords :: [(Int,Int)] -> [((Int,Int), (Int,Int))]
sidesCoords xs = zip xs (revFirst xs)

lenSides :: [((Int,Int),(Int,Int))] -> [Float]
lenSides [] = []
lenSides ((x,xx):xxs) = pythagoras x xx : lenSides xxs 
--    where (x,xs) = xs

perimeter :: [(Int,Int)] -> Float 
perimeter = sum . lenSides . sidesCoords

-- calculates the perimeter of a polygon given its sides
-- as coordinates on a separate line where a side is represented
-- by a pair of adjacent coordinates (from left to right)
--
-- how to use:
-- λ> main 
--    4 
--    1 0
--    0 1
--    0 0
--    1 1
-- returns: 4.0

main :: IO ()
main =
    do 
        n <- readLn
        input <- replicateM n getLine
        
        let coordlist = coord . intlist . map words 
    
        print $ perimeter $ coordlist input
        
