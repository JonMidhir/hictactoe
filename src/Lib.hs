module Lib
    ( initiateGame
    ) where

import Data.List
import Data.List.Split
import Data.Char

data Player = Player { name :: String, character :: Char } deriving (Show, Eq)
data Square = Square { position :: Position, occupant :: Maybe Player } deriving (Show)

type Position = (Int, Int)
type Sequence = [Position]
type Board = [Square]
type BoardLine = (Square, Square, Square)

-- The default board state in a new game
initialBoard :: Board
initialBoard = [Square (0,0) Nothing, Square (0,1) Nothing, Square (0,2) Nothing,
                Square (1,0) Nothing, Square (1,1) Nothing, Square (1,2) Nothing,
                Square (2,0) Nothing, Square (2,1) Nothing, Square (2,2) Nothing]

-- Sequences of positions that can win the game
winningSequences :: [Sequence]
winningSequences = [[(0,0), (0,1), (0,2)], [(1,0), (1,1), (1,2)], [(2,0), (2,1), (2,2)],
                    [(0,0), (1,0), (2,0)], [(0,1), (1,1), (2,1)], [(0,2), (1,2), (2,2)],
                    [(0,0), (1,1), (2,2)], [(0,2), (1,1), (2,0)]]

-- Get the row from a square
squareRow :: Square -> Int
squareRow (Square (a, _) _) = a + 1

-- Get the value in a square
squareValue :: Square -> Char
squareValue (Square _ Nothing) = ' '
squareValue (Square _ (Just player)) = character player

-- Return the lines in a given board
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

-- -- Figure out if a player has won
-- boardWonBy :: Player -> Board -> Boolean
-- boardWonBy player squares =
--   map (\(a,b,c) -> (squareAtPosition a squares, squareAtPosition b squares, squareAtPosition c squares)) winningSequences
--   -- filter (\(Square position _) -> position ==) squares

-- Return the square at a given position. Could produce a runtime error if given
-- a position that is out of bounds. This won't happen as the winningSequences
-- data is hardcoded.
squareAtPosition :: Board -> Position -> Square
squareAtPosition (square:[]) (x, y) = square
squareAtPosition (square:xs) pos
  | pos == (position square) = square
  | otherwise                = squareAtPosition xs pos

-- Transform a list of positions into a sequence of squares
toBoardSequence :: Board -> Sequence -> [Square]
toBoardSequence board positions = map (squareAtPosition board) positions

-- Return all winnablePermutations of lists of Squares on the board
winnablePermutations :: Board -> [[Square]]
winnablePermutations squares = map (toBoardSequence squares) winningSequences

-- Determine if a given permutation of the board is won
permutationWon :: [Square] -> Bool
permutationWon squares = (permutationWinner squares) /= Nothing

permutationWinner :: [Square] -> Maybe Player
permutationWinner permutation
  | length occupants > 1   = Nothing
  | occupants == [Nothing] = Nothing
  | otherwise              = head occupants
  where occupants = nub $ map occupant permutation

boardWon :: Board -> Bool
boardWon = any permutationWon . winnablePermutations

-- Play move if matching
playMatchingMove :: Position -> Player -> Square -> Square
playMatchingMove position player (Square position' _)
  | position' == position = Square position' (Just player)
  | otherwise             = Square position' Nothing

-- Plays a given move by a player
playMove :: Position -> Player -> Board -> Board
playMove position player = map (playMatchingMove position player)

getMove :: IO Position
getMove = do
  putStrLn "Hello, make a move! (x, y)"
  move <- getLine

  return $ read move :: IO Position

-- Debug method to show current winable state statuses
winnableStatesProgress :: Board -> String
winnableStatesProgress = unlines . map show . winnablePermutations

initiateGame :: IO ()
initiateGame = do
  let player1 = Player "John" 'x'
  putStrLn $ winnableStatesProgress initialBoard
  pos <- getMove

  let newBoard = playMove pos player1 $ initialBoard
  putStrLn $ boardStr newBoard

  if boardWon newBoard
  then putStrLn "Won"
  else putStrLn "No win"

  -- putStrLn $ winnableStatesProgress newBoard
