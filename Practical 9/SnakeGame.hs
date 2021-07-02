module Main where

import System.Process (system) -- for clearing the terminal
import System.IO


--------------------------------------------------------------------
-- Windows and Mac/Linux differ regarding terminal behaviour.
-- So we have a technical bit here. Comment in/out the parts for 
-- your platform.

-- Windows-only:
-- The following code makes a Haskell wrapper around the
-- Windows-specific C function getch in order to work around the
-- problem of getChar waiting for enter key on Windows platforms.

-- Comment this section out on Linux/Mac
{-# LANGUAGE ForeignFunctionInterface #-}
import Data.Char
import Foreign.C.Types
getPlatformDependentChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

-- Comment this out on Windows
-- getPlatformDependentChar = getChar


-- Clear the screen: Different for Windows and Linux/Mac
cls :: IO ()
cls = 
  do 
    exitCode <- system "cls" -- Use this for Windows
    -- exitCode <- system "clear" -- Use this for Linux/Mac
    return ()



-----------------------------------------------------------------
-- Data types for the game

-- A Game is either an exit state or it's a Board containing some
-- snake locations and some food locations
data Game = Lost | Won | Board SnakeLocs FoodLocs 


type SnakeLocs  = Locs
type FoodLocs   = Locs
type Locs       = [Loc]
type Loc        = Int


-- The board will be 20 x 20 characters (so 400 locations)
dimension = 20 


----------------------------------------------------------------
-- initial board

-- The snake has its head at location 45, and its tail from 46 to 50.
-- The food is scattered at 12, 67, and 156.

initialBoard :: Game
initialBoard = Board [45,46,47,48,49,50] [12,67,156]



----------------------------------------------------------------
-- Functions to manipulate a Game state

isExit :: Game -> Bool
isExit Lost = True
isExit Won  = True
isExit _    = False


convertWin (Board s []) = Won
convertWin b            = b


-- snakeHead should return the location of the head of the snake
snakeHead :: Game -> Loc
snakeHead (Board (h:t) fs) = h



-- update takes two parameters:
--   the location we're trying to move the head of the snake to
--   the current state of the game
-- and produces an updated state of the game

-- If we've reached the edge of the board, then make no change
-- If the head is over a food square, grow the snake, remove the food
-- If the head is over the snake tail, end the game
-- Otherwise, just move the snake (add to head, remove from tail)

update :: Loc -> Game -> Game
update newheadpos (Board (h:ts) fs) = 
    if newheadpos < 0 || newheadpos > (dimension*dimension) 
        then Board (h:ts) fs
    else if newheadpos `elem` fs 
        then convertWin (Board (newheadpos:h:ts) (removeLoc newheadpos fs))
    else if newheadpos `elem` ts 
        then Lost
    else Board (newheadpos:h:removeLastLoc ts) fs  
                                       

----------------------------------------------------------------
-- Functions to manipulate a locations list 


-- removeLastLoc will remove the last location in a list. If the list
-- is empty, this function should not error, but instead should return
-- an empty list.
removeLastLoc :: Locs -> Locs 
removeLastLoc [] = []
removeLastLoc xs = init xs


-- removeLoc will remove the location from the list of locations.
removeLoc :: Loc -> Locs -> Locs
removeLoc x [] = []
removeLoc x (y:ys) = 
  if x == y then ys else y:removeLoc x ys





----------------------------------------------------------------
-- Output

writeGame :: Game -> IO ()
writeGame Lost = putStrLn "Game over"
writeGame Won  = putStrLn "You have won!"
writeGame board =
    do 
      writeBoardEdge
      writeBoardDetail board
      writeBoardEdge


writeBoardEdge = 
      putStrLn (take (dimension+2) (repeat '-'))


writeBoardDetail :: Game -> IO ()
writeBoardDetail board = 
    let squares = [drawSquare i board | i <- [1..(dimension*dimension)]] 
        rows = makeRows squares
    in 
      writeAllRows rows


writeAllRows :: [String] -> IO ()
writeAllRows [] = return ()
writeAllRows (r:rows) = 
      do 
        putStrLn  r
        writeAllRows rows


drawSquare :: Loc -> Game -> Char
drawSquare i (Board snake food)  
    | i == head snake     = '@'
    | i `elem` tail snake = '#'
    | i `elem` food       = '+'
    | otherwise           = ' '


-- Takes the characters for the squares of the board, chops them up into rows 
-- and flanks each row by vertical bars
makeRows :: String -> [String]
makeRows [] = []
makeRows xs = 
   ("|" ++ row ++ "|") : makeRows rest
   where
     row = take dimension xs
     rest = drop dimension xs



--------------------------------------------------------------------------------------
-- Okay, let's play!


play :: Game -> IO ()
play g =
    if isExit g
      then 
        writeGame g
      else do
        cls
        writeGame g
        c <- getPlatformDependentChar
        let 
          snakehd = snakeHead g 
          newG
            | c == 'l' = update (snakehd - 1) g
            | c == 'r' = update (snakehd + 1) g
            | c == 'd' = update (snakehd + dimension) g
            | c == 'u' = update (snakehd - dimension) g
            | otherwise = g
        play newG


main :: IO ()
main =
  do
    hSetBuffering stdin NoBuffering
    play initialBoard


