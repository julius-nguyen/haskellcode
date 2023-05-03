data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune deriving (Eq)

table :: [(Planet,Float)]
table = [(Mercury,0.2408467),
         (Venus,0.61519726),
         (Earth,1.0),
         (Mars,1.8808158),
         (Jupiter,11.862615),
         (Saturn,29.447498),
         (Uranus,84.016846),
         (Neptune,164.79132)]

-- converts seconds to earth years 
secToEarthYears :: Float -> Float 
secToEarthYears t = t / 31557600 

find :: (Eq a) => a -> [(a,b)] -> b 
find e [] = error "value not found"
find e ((k,v):xs) 
  | k == e = v 
  | otherwise = find e xs 

-- converts earth years to orbital periods
toOrbitalPeriods :: Planet -> Float -> Float 
toOrbitalPeriods planet n = n / (find planet table)

ageOn :: Planet -> Float -> Float
ageOn planet = toOrbitalPeriods planet . secToEarthYears
