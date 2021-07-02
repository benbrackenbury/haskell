import Data.List
import Data.Char

-- module Main where 


-- main = 
--   do
--     text <- readFile "Wales.txt"
--     let cs = wordFreqs text
--         nations   = lookupCounts cs ["wales","scotland","england","ireland"]
--         questions = lookupCounts cs ["when","how","what"]
--         members   = lookupCounts cs ["gentleman","lady"]
--         agreement = lookupCounts cs ["disagree","agree"]
--     outputCounts nations
--     outputCounts questions
--     outputCounts members
--     outputCounts agreement


------------------------------------------------------------
-- Question 1

removePunctuation :: String -> String
removePunctuation (x:xs) = [x | x <- xs, not (elem x "?!:,.-()\"\'") ]


------------------------------------------------------------
-- Question 2

-- wordFreqs :: String -> [(String, Int)]
wordFreqs (x:xs) = 
  sort ( words (removePunctuation (toLowerString (x: xs))))
  where
    toLowerString :: [Char] -> [Char]
    toLowerString str = [ toLower x | x <- str]


------------------------------------------------------------
-- Question 3

-- lookupCounts :: [(String, Int)] -> [String] -> [(String, Int)]



------------------------------------------------------------
-- Question 4

-- outputCounts :: [(String, Int)] -> IO()
