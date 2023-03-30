module HappinessScore where 

import qualified Data.Set as Set 

setA :: Set.Set Int 
setA = Set.fromList [3,1]

setB :: Set.Set Int
setB = Set.fromList [5,7]

numbers :: [Int]
numbers = [1,5,3]

happiness :: [Int] -> Set.Set Int -> Set.Set Int -> Int 
happiness [] _ _ = 0
happiness (n:ns) set1 set2
  | Set.member n set1 = 1 + happiness ns set1 set2 
  | Set.member n set2 = -1 + happiness ns set1 set2 
  | otherwise = happiness ns set1 set2

--happinessF :: [Int] -> Set.Set Int -> Set.Set Int -> Int 
--happinessF set1 set2 = foldr ((+) . cond) 0
--  where cond x
--            | Set.member x set1 = 1
--            | Set.member x set2 = -1
--            | otherwise = 0
