module Main where

import Types
  ( Element (..)
  , Line
  , Board
  , Game (..)
  , Position
  , Input
  , toGameElement
  , Terminals (..)
  )

import Data.Bool (bool)
import Data.List (intersperse, findIndex)
import Data.List.Split (endBy)
import Data.Maybe (fromJust)
import Control.Monad (guard)

main :: IO ()
main = allLevels >>= runGame

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


runGame :: [Board] -> IO ()
runGame []       = putStrLn "No more levels"
runGame l@(x:xs) = do
  let level = initLevel x
  status <- play level
  case status of
    Solved    -> runGame xs
    Restarted -> runGame l
    Stopped   -> putStrLn "Stopped"

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

play :: Game -> IO Terminals
play g = do
  input <- getValidPlayerInput
  pure $ case input of
    "q" -> Stopped
    "r" -> Restarted
    _   -> handleMovement input g
    

getValidPlayerInput :: IO String
getValidPlayerInput = do
  input <- getLine
  if isValidInput input
    then pure input
    else putStrLn "invalid, please try again" >> getValidPlayerInput

isValidInput :: Input -> Bool
isValidInput i = i `elem` ["w", "s", "a", "d", "q", "r"]

getElement :: Position -> Input -> Board -> Element
getElement (line, col) "u" board = (board !! (line - 1)) !! col -- ^ previous line, same col
getElement (line, col) "d" board = (board !! (line + 1)) !! col -- ^ next line, same col
getElement (line, col) "l" board = (board !! line) !! (col - 1)
getElement (line, col) "r" board = (board !! line) !! (col + 1)

handleMovement :: String -> Game -> Terminals
handleMovement = undefined