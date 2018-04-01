module Main where

import Types
  ( Element (..)
  , Line
  , Board
  , Game (..)
  , Position
  , Input
  )

import Data.Bool (bool)
import Data.List (intersperse, findIndex)
import Data.List.Split (endBy)
import Data.Maybe (fromJust)
import Control.Monad (guard)

main :: IO ()
main = do
  putStrLn "hello world"

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


