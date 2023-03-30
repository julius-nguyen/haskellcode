module HappinessScore where 

import qualified Data.Set as Set 

setA :: Set.Set Int 
setA = Set.fromList [3,1]

setB :: Set.Set Int
setB = Set.fromList [5,7]

numbers :: [Int]
numbers = [1,5,3]

happinessF :: Set.Set Int -> Set.Set Int -> [Int] -> Int 
happinessF set1 set2 = foldr ((+) . cond) 0 (x:xs)
  where cond x
            | Set.member x set1 = 1
            | Set.member x set2 = -1
            | otherwise = 0
