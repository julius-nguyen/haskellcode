module FizzBuzz where 
    
data FizzBuzz = FizzBuzzN Int | FizzBuzzStr String deriving Show

numbers :: [FizzBuzz]
numbers = [ FizzBuzzN x | x <-[1..100]]

divisibleByThree :: FizzBuzz -> Bool 
divisibleByThree (FizzBuzzN n) = n `mod` 3 == 0

divisibleByFive :: FizzBuzz -> Bool
divisibleByFive (FizzBuzzN n) = n `mod` 5 == 0

fizzbuzz :: [FizzBuzz] -> [FizzBuzz]
fizzbuzz [] = []
fizzbuzz (x:xs) 
    | divisibleByThree x && divisibleByFive x = FizzBuzzStr "FizzBuzz" : fizzbuzz xs
    | divisibleByThree x = FizzBuzzStr "Fizz" : fizzbuzz xs
    | divisibleByFive x = FizzBuzzStr "Buzz" : fizzbuzz xs
    | otherwise = x : fizzbuzz xs

