module Lib
    ( initiateGame
    ) where

import Data.List
import Data.List.Split
import Data.Char
import Text.Regex.Posix

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

boardFull :: Board -> Bool
boardFull board = all (/= Nothing) $ map occupant board

-- Play move if matching
playMatchingMove :: Position -> Player -> Square -> Square
playMatchingMove position player (Square position' occupant)
  | position' == position = Square position' (Just player)
  | otherwise             = Square position' occupant

-- Plays a given move by a player
playMove :: Position -> Player -> Board -> Board
playMove position player = map (playMatchingMove position player)

-- GRMA Bryan O'Sullivan - http://www.serpentine.com/blog/2007/02/27/a-haskell-regular-expression-tutorial/
translateMove :: String -> Maybe Position
translateMove ('A':y:[]) = translateMove $ '0':y:[]
translateMove ('B':y:[]) = translateMove $ '1':y:[]
translateMove ('C':y:[]) = translateMove $ '2':y:[]
translateMove (x:y:[]) = Just ((digitToInt y) - 1, digitToInt x)
translateMove _ = Nothing

getMove :: IO Position
getMove = do
  putStrLn "Make a move! (e.g. A1, C2, etc)"
  move <- getLine
  let position = translateMove move

  case position of
    Nothing -> getMove
    Just a -> return a

playGame :: Board -> Player -> IO Board
playGame board player = do
  putStrLn $ boardStr board
  pos <- getMove

  let newBoard = playMove pos player board

  if (boardWon newBoard) || (boardFull newBoard)
    then return newBoard
    else playGame newBoard player

initiateGame :: IO ()
initiateGame = do
  let player1 = Player "John" 'x'

  finalBoard <- playGame initialBoard player1
  putStrLn $ boardStr finalBoard

  if boardWon finalBoard
  then putStrLn "Won"
  else putStrLn "No win"
