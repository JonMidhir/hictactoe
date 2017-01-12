module Lib
    ( initiateGame
    ) where

import Data.List
import Data.List.Split
import Data.Char

-- Player Name Character
data Player = Player String Char deriving (Show)
data Square = Square Position (Maybe Player) deriving (Show)

type Position = (Int, Int)
type Board = [Square]
type BoardLine = (Square, Square, Square)

-- The default board state in a new game
initialBoard :: Board
initialBoard = [Square (0,0) Nothing, Square (0,1) Nothing, Square (0,2) Nothing,
                Square (1,0) Nothing, Square (1,1) Nothing, Square (1,2) Nothing,
                Square (2,0) Nothing, Square (2,1) Nothing, Square (2,2) Nothing]

-- Get the row from a square
squareRow :: Square -> Int
squareRow (Square (a, _) _) = a + 1

-- Get the value in a square
squareValue :: Square -> Char
squareValue (Square (_, _) Nothing) = ' '
squareValue (Square (_, _) (Just (Player _ a))) = a

-- Get the position (Int, Int) of a Square
squarePosition :: Square -> Position
squarePosition (Square position _) = position

-- Return the lines in a given board. BoardLine type could be changed to use
-- list of Squares instead of tuples to simplify this.
boardLines :: Board -> [BoardLine]
boardLines = map (\(x:y:z:[]) -> (x, y, z)) . chunksOf 3

-- Show a boardline as a string
boardLineStr :: BoardLine -> String
boardLineStr (a, b, c) =
  (rowChar:) . ("  " ++) . (intersperse ' ') . (intersperse '|') $ squareValues
  where rowChar = intToDigit $ squareRow a
        squareValues = map squareValue [a, b, c]

-- The gridspacer string for a standard board
gridSpacer :: String
gridSpacer = "  ---+---+---"

-- Presents a Board as a String
boardStr :: Board -> String
boardStr board = unlines $ ("   A   B   C ":) $ intersperse gridSpacer lineStrings
  where lineStrings = map boardLineStr $ boardLines board

-- Play move if matching
playMatchingMove :: Position -> Player -> Square -> Square
playMatchingMove position player (Square position' _)
  | position' == position = Square position' (Just player)
  | otherwise             = Square position' Nothing

-- Plays a given move by a player
playMove :: Position -> Player -> Board -> Board
playMove position player = map (playMatchingMove position player)

initiateGame :: IO ()
initiateGame = do
  let player1 = Player "John" 'x'
  putStrLn $ boardStr $ playMove (1,2) player1 initialBoard
