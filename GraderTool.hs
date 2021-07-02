module Main where

import StudentsGrades (showStudentGrades, gradeMessage, makeStudentGrade, StudentGrade)
import Data.Maybe (fromJust)

-- You don't need to make any changes in this file.
-- Don't worry about this code. We won't cover IO until next week. 
-- Make all edits in the StudentsGrades.hs file.

main :: IO()
main = 
  do
    gs <- mapM getGrade [1..3]      -- get 3 Maybe StudentGrades into a list
    let gs' = map fromJust (filter (/= Nothing) gs) -- remove the bad ones
    putStrLn (showStudentGrades gs')  -- display them
    queryForever gs'                  -- query them


getGrade n = 
   do 
     putStrLn "Input a username:"
     username <- getLine
     putStrLn "Input a grade (0-100):"
     grade    <- getLine
     return (makeStudentGrade username grade)


queryForever gs =
  do
    putStrLn "Which student would you like to look up?"
    name <- getLine
    putStrLn (gradeMessage name gs)
    queryForever gs   -- recurse again to create an infinite loop
 

