module Main where

import System.IO

-- An example of how we might use an infinite data structure
data Maze = Junction String Maze Maze 
            | MazeExit
            deriving Show

-- A very simple maze
verySimpleMaze = Junction "start" MazeExit MazeExit

-- We create a maze that is recursively defined. Note that hole is 
-- particularly recursive.
mazeOfDoom = entrance
  where
    entrance = Junction "entrance" forest swamp
    forest   = Junction "forest" MazeExit river
    swamp    = Junction "swamp" river hole
    river    = Junction "river" forest swamp
    hole     = Junction "hole" hole hole

-- moveLeft and moveRight return a maze that results from a move
moveLeft, moveRight :: Maze -> Maze
moveLeft MazeExit = error "Can't move left from Exit"
moveLeft  (Junction here left right) = left
moveRight MazeExit = error "Can't move right from Exit"
moveRight (Junction here left right) = right

-- We can print our current location
showLoc :: Maze -> String
showLoc MazeExit = "Exit"
showLoc (Junction here left right) =
   "You are at the " ++ here ++ "."


isExit :: Maze -> Bool
isExit MazeExit = True
isExit _        = False



-------------------------------------------------------
-- IO code. We will cover IO next week.


-- wander displays current location and checks for exit.
-- If not exit then we can ask user where to move in maze

wander :: Maze -> IO()
wander m = 
  do 
     putStrLn (showLoc m)
     if isExit m  
       then return ()
       else moveInMaze m


-- moveInMaze prompts user for a character and takes a new portion of 
-- the infinite maze data structure accordingly. We then call wander 
-- again with the new maze

moveInMaze :: Maze -> IO()
moveInMaze m =
  do      
     c <- getChar
     putStrLn ""
     let newM 
           | c == 'l'  = moveLeft m
           | c == 'r'  = moveRight m 
           | otherwise = m
     wander newM


main :: IO()
main =
  do
    hSetBuffering stdin NoBuffering
    wander mazeOfDoom
