module Sokoban.Internals.Utils where

import Sokoban.Internals.Types

advance :: Position -> Direction -> Position
advance (r, c) "w" = (r - 1, c)
advance (r, c) "s" = (r + 1, c)
advance (r, c) "a" = (r, c - 1)
advance (r, c) "d" = (r, c + 1)

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

isGoalElement :: Element -> Bool
isGoalElement e = e == Goal || e == BoxOnGoal || e == PlayerOnGoal

checkGameSolved :: Game -> Bool
checkGameSolved game =
  let board = currentBoard game in and $ fmap (notElem Box) board

getElement :: Position -> Board -> Element
getElement (row, col) board = (board !! row) !! col