module Main where

import Data.List  (findIndex)
import Data.Maybe (fromJust)
import System.IO

import Sokoban.Internals.Types

import Sokoban.Parse
import Sokoban.Input
import Sokoban.Move

main :: IO ()
main = hSetBuffering stdout NoBuffering >> allLevels >>= runGame

allLevels :: IO [Board]
allLevels = do
  levels <- readFile "data/level1.txt"
  let lineList = lines levels
  return $ parseBoards lineList

runGame :: [Board] -> IO ()
runGame []       = putStrLn "No more levels"
runGame l@(x:xs) = do
  let level = initLevel x
  status <- play level
  case status of
    Solved ->
      putStrLn "\n\nYou Rock!!\nOn to the next one!!!\n\n" >> runGame xs
    Restarted -> runGame l
    Stopped   -> putStrLn "Stopped"

play :: Game -> IO Terminals
play g = do
  print g >> putStrLn "\n"
  putStr "Enter a direction: "
  input <- getValidPlayerInput
  case input of
    "q" -> pure Stopped
    "r" -> pure Restarted
    dir ->
      let newState = handleMovement dir g
      in  if checkGameSolved newState then pure Solved else play newState

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

checkGameSolved :: Game -> Bool
checkGameSolved game =
  let board = currentBoard game in and $ fmap (notElem Box) board


