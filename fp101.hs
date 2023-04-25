
-- implementation of the FP101 (Github Guides) examples in Haskell 
-- https://github.com/readme/guides/functional-programming-basics

-- instead of having a list with different data types within a list (this is actually not permitted in Haskell)
-- we need to define an own data type that has the student's name and GPA
data StudentRecord = Student Name GPA deriving (Show)  
type Name = String 
type GPA = Float

-- then we can define a list of students
students :: [StudentRecord]
students = [Student "Jacklyn Ford" 3.95,
            Student "Cassidy Williams" 4.0,
            Student "Joe Randy" 2.2]

-- changes the GPA of a student given a change value
changeGPA :: Float -> StudentRecord -> StudentRecord 
changeGPA change (Student name gpa) = Student name (gpa+change)

-- increases all GPAs in a list by the same factor
-- in order to do that, we apply the changeGPA function to each element of a list using map
changeGPAs :: Float -> [StudentRecord] -> [StudentRecord]
changeGPAs change xs = map (changeGPA change) xs  
