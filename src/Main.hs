module Main where

import Data.Bool (bool)
import Data.List (intersperse, findIndex)
import Data.List.Split (endBy)
import Data.Maybe (fromJust)
import Control.Monad (guard)

main :: IO ()
main = do
  putStrLn "hello world"

data Element = Wall
             | Player
             | PlayerOnGoal
             | Box
             | BoxOnGoal
             | Goal
             | Floor deriving (Eq)

instance Show Element where
  show Wall         = "#"
  show Player       = "@"
  show PlayerOnGoal = "+"
  show Box          = "$"
  show BoxOnGoal    = "*"
  show Goal         = "."
  show Floor        = " "

{-
each line on the board is a list of elements i.e
#### ^-- line 1
#@.# ^-- line 2
#$ # ^-- line 3 etc
#  #
# $#
#. #
#  #
####

the board is a list of lines
-}
type Line = [Element]
type Board = [Line]
data Game = Game  { currentBoard :: Board,
                    playerPosition :: (Int, Int)
                  }
instance Show Game where
  show game = concat boardLines
    where
      -- ["##", "##"] -> ["##", "\n", "##"]
      boardLines = intersperse "\n" board'
      -- [[Wall, Wall]] -> [["#", "#"]] -> [["##"]]
      board' = concatMap show <$> currentBoard game

toGameElement :: Char -> Element
toGameElement '#' = Wall
toGameElement '@' = Player
toGameElement '+' = PlayerOnGoal
toGameElement '$' = Box
toGameElement '*' = BoxOnGoal
toGameElement '.' = Goal
toGameElement ' ' = Floor

allLevels :: IO [Board]
allLevels = do
  levels <- readFile "data/level1.txt"
  let lineList = lines levels
  return $ read' lineList

read' :: [String] -> [Board]
read' = (:) <$> readOne <*> readRest

-- parse a level from the list of lines
readOne :: [String] -> Board
readOne =
  fmap (fmap toGameElement) . endBy "\r" . concat . takeWhile (/= "\r") . drop 1


readRest :: [String] -> [Board]
readRest left | null left = []
              | otherwise = read' . tail . dropWhile (/= "\r") $ left


initLevel :: Board -> Game
initLevel board = Game {currentBoard = board, playerPosition = go 0 board}
 where
  -- if the player is on the line indicated by the index, find column
  -- else check the next line
  go :: Int -> Board -> Position
  go index (x:xs) = if Player `elem` x || PlayerOnGoal `elem` x
    then (index, findCol x)
    else go (index + 1) xs
  findCol = fromJust . findIndex (\e -> e == Player || e == PlayerOnGoal)


type Input = String
type Position = (Int, Int)

play :: Game -> IO ()
play game = do
  input    <- getPlayerInput
  newState <- move input (playerPosition game) (currentBoard game)
  pure ()

getPlayerInput :: IO String
getPlayerInput = do
  input <- getLine
  if isValidInput input
    then pure input
    else putStrLn "invalid, please try again" >> getPlayerInput

isValidInput :: Input -> Bool
isValidInput i = i `elem` ["u", "d", "l", "r"]


move :: Monad m => Input -> Position -> Board -> m Game
move = undefined


getElement :: Position -> Input -> Board -> Element
getElement (line, col) "u" board = (board !! (line - 1)) !! col -- ^ previous line, same col
getElement (line, col) "d" board = (board !! (line + 1)) !! col -- ^ next line, same col
getElement (line, col) "l" board = (board !! line) !! (col - 1)
getElement (line, col) "r" board = (board !! line) !! (col + 1)


