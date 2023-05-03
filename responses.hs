import Data.Char (isUpper,isDigit)

isUpperText :: String -> Bool
isUpperText = foldr ((&&) . isUpper) True

elimPunct :: String -> String 
elimPunct = filter (/= '!') . filter (/= ' ') . filter (/= '?'). filter (/= '.') . filter (/= ',')

elimNumbers :: String -> String 
elimNumbers = filter (not . isDigit)

isQuestion :: String -> Bool 
isQuestion xs = last xs == '?' 

isWhitespace :: String -> Bool
isWhitespace = foldr ((&&) . (==' ')) True

responseFor :: String -> String
responseFor xs 
  | (isUpperText onlyWords) && (isQuestion xs) = "Calm down, I know what I'm doing!"
  | isUpperText onlyWords = "Whoa, chill out!"
  | isQuestion xs = "Sure."
  | isWhitespace xs = "Fine. Be that way!"
  | otherwise = "Whatever."
  where onlyWords = elimNumbers $ elimPunct xs
