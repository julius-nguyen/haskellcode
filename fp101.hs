
-- implementation of the FP101 (Github Guides) examples in Haskell 
-- https://github.com/readme/guides/functional-programming-basics

data StudentRecord = Student Name GPA deriving (Show)  
type Name = String 
type GPA = Float

students :: [StudentRecord]
students = [
  Student "Jacklyn Ford" 3.95,
  Student "Cassidy Williams" 4.0,
  Student "Joe Randy" 2.2]

-- changes the GPA of a student given a change value
changeGPA :: Float -> StudentRecord -> StudentRecord 
changeGPA change (Student name gpa) = Student name (gpa+change)

-- increases all GPAs in a list by the same factor
changeGPAs :: Float -> [StudentRecord] -> [StudentRecord]
changeGPAs change xs = map (changeGPA change) xs  
