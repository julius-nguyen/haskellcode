import Control.Monad

flatten :: [[a]] -> [a]
flatten [] = []
flatten (xs:xxs) = xs ++ flatten xxs

-- calculates the sum of the values of multiple arrays
-- first input specifies the number N of arrays
-- on separate lines we can then enter the array values separated each by white space
main :: IO ()
main =
    do 
        n <- readLn
        input <- replicateM n getLine
        let intlist = map read (flatten (map words input))
        putStrLn (show (sum intlist))
