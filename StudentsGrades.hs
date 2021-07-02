module StudentsGrades (showStudentGrades, gradeMessage, makeStudentGrade, StudentGrade) where

import Data.Char (isDigit)


type Grade = Int
type Username = String
type StudentGrade = (Username, Grade)

-- The showStudentGrades function should take a list of StudentGrade
-- items and produce one single string, to nicely display all of them.

showStudentGrades :: [StudentGrade] -> String
showStudentGrades [] = []
showStudentGrades ((u,g):gs) =
    u ++ ": " ++ show g ++  "\n" ++ showStudentGrades gs




-- The gradeMessage function takes two arguments:
--  a username to look up
--  a list of StudentGrades in which to look
-- and it produces a suitable message, with the corresponding grade
-- if it has been found, and a helpful response if it's not found.
-- Use the 'lookup' function from the Prelude to help you.

gradeMessage :: String -> [StudentGrade] -> String
gradeMessage u gs =
    if lookup u gs == Nothing
        then "There is no grade for " ++ u
        else "The grade for " ++ u ++ " is " ++ show (lookup u gs)





-- The makeStudentGrade function should take two arguments: 
-- a string which is the username, 
-- another string containing the grade, 
-- and it should produce a Maybe StudentGrade type of result

-- The function should check that the string representing the grade
-- consists only of characters that are digits (either use the 'isDigit'
-- function from Data.Char, or write your own). If this is not the
-- case, this function should return Nothing. If the string is ok,
-- then use the 'read' function to convert the String to a Grade, 
-- and return the result, wrapped in a Just constructor.

makeStudentGrade :: String -> String -> Maybe StudentGrade
makeStudentGrade u g =
    if (all isDigit g)
        then Just (u, read g)
        else
            Nothing