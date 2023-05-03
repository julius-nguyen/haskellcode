isLeapYear :: Integer -> Bool 
isLeapYear year = case (year `mod` 4) of 
                    0 -> case (year `mod` 100) of 
                           0 -> case (year `mod` 400) of 
                                  0 -> True 
                                  _ -> False
                           _ -> True 
                    _ -> False
