module Lib
    ( initiateGame
    ) where

import Data.List
import Data.List.Split
import Data.Char

data Square = Square ((Int ,Int), Char) deriving (Show)
data Board = Board [Square] deriving (Show)
data BoardLine = BoardLine (Square, Square, Square) deriving (Show)

-- Player Name Character
data Player = Player String Char

-- The default board state in a new game
initialBoard :: Board
initialBoard = Board [Square ((0,0), ' '), Square ((0,1), ' '), Square ((0,2), ' '),
                      Square ((1,0), ' '), Square ((1,1), ' '), Square ((1,2), ' '),
                      Square ((2,0), ' '), Square ((2,1), ' '), Square ((2,2), ' ')]

-- Get the row from a square
squareRow :: Square -> Int
squareRow (Square ((a, _), _)) = a + 1

-- Get the value in a square
squareValue :: Square -> Char
squareValue (Square ((_, _), a)) = a

-- Return the lines in a given board. BoardLine type could be changed to use
-- list of Squares instead of tuples to simplify this.
boardLines :: Board -> [BoardLine]
boardLines (Board (squares)) =
  map BoardLine $ map (\(x:y:z:[]) -> (x, y, z)) $ chunksOf 3 squares

-- Show a boardline as a string
boardLineStr :: BoardLine -> String
boardLineStr (BoardLine (a, b, c)) =
  ((intToDigit rowNum):) . ("  " ++) . (intersperse ' ') . (intersperse '|') $ map squareValue [a, b, c]
  where rowNum = squareRow a

-- The gridspacer string for a standard board
gridSpacer :: String
gridSpacer = "  ---+---+---"

-- Presents a Board as a String
boardStr :: Board -> String
boardStr board = unlines $ ("   A   B   C ":) $ intersperse gridSpacer boardLineStrings
  where boardLineStrings = map boardLineStr $ boardLines board

initiateGame :: IO ()
initiateGame = do
  putStrLn $ boardStr initialBoard
