module Main where

import Types
  ( Element (..)
  , Line
  , Board
  , Game (..)
  , Position
  , Direction
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
    Solved ->
      putStrLn "\n\nYou Rock!!\nOn to the next one!!!\n\n" >> runGame xs
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
  print g >> putStrLn "\n"
  putStr "Enter a direction: "
  input <- getValidPlayerInput
  case input of
    "q" -> pure Stopped
    "r" -> pure Restarted
    dir ->
      let newState = handleMovement dir g
      in  if checkGameSolved newState then pure Solved else play newState

getValidPlayerInput :: IO String
getValidPlayerInput = do
  input <- getLine
  if isValidInput input
    then pure input
    else putStrLn "invalid, please try again" >> getValidPlayerInput

isValidInput :: String -> Bool
isValidInput i = i `elem` ["w", "s", "a", "d", "q", "r"]

getElement :: Position -> Board -> Element
getElement (row, col) board = (board !! row) !! col

handleMovement :: Direction -> Game -> Game
handleMovement dir game
  | isEmpty dir game    = walk Player (playerPosition game) dir game
  | isPushable dir game = push dir game
  | otherwise           = game

isEmpty :: Direction -> Game -> Bool
isEmpty dir game =
  let board  = currentBoard game
      pos    = playerPosition game
      newPos = advance pos dir
  in  isEmptyElement $ getElement newPos board
isPushable :: Direction -> Game -> Bool
isPushable dir game =
  let board   = currentBoard game
      pos     = playerPosition game
      newPos1 = advance pos dir -- new position of player after pushing
      e1      = getElement newPos1 board
      newPos2 = advance newPos1 dir -- new position of box after pushing
      e2      = getElement newPos2 board
  in  isBoxElement e1 && isEmptyElement e2

isPlayerElement :: Element -> Bool
isPlayerElement e = e == Player || e == PlayerOnGoal

isEmptyElement :: Element -> Bool
isEmptyElement e = e == Floor || e == Goal

isBoxElement :: Element -> Bool
isBoxElement e = e == Box || e == BoxOnGoal

advance :: Position -> Direction -> Position
advance (r, c) "w" = (r - 1, c)
advance (r, c) "s" = (r + 1, c)
advance (r, c) "a" = (r, c - 1)
advance (r, c) "d" = (r, c + 1)

-- | Walk changes the position of a player or a box
walk :: Element -> Position -> Direction -> Game -> Game
walk e p dir game =
  let curPosition@(r , c ) = p
      nextPostion@(r', c') = advance curPosition dir
      board                = currentBoard game
      newBoard             = if c == c'
        then updateRow e (curPosition, nextPostion) board -- ^ horizontal movement
        else updateCol e (curPosition, nextPostion) board --   verical movement
  in  Game {currentBoard = newBoard, playerPosition = nextPostion}
  -- ^ the player position is being changed no matter the element moved
  --   this is not a problem when moving a box because walk is called again to move the player

push :: Direction -> Game -> Game
push = undefined

checkGameSolved :: Game -> Bool
checkGameSolved = undefined

updateRow :: Element -> (Position, Position) -> Board -> Board
updateRow = undefined

updateCol :: Element -> (Position, Position) -> Board -> Board
updateCol = undefined
