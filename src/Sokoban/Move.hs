module Sokoban.Move (handleMovement) where

import Sokoban.Internals.Types
import Sokoban.Internals.Utils

handleMovement :: Direction -> Game -> Game
handleMovement dir game
  | isEmpty dir game    = move Player (playerPosition game) dir game
  | isPushable dir game = push dir game
  | otherwise = game

-- | Changes the position of a player or a box
move :: Element -> Position -> Direction -> Game -> Game
move e p dir game =
  let curPosition@(r , c ) = p
      nextPostion@(r', c') = advance curPosition dir
      board                = currentBoard game
      updatedBoard         = if c == c'
        then updateRow e (curPosition, nextPostion) board -- horizontal movement
        else updateCol e (curPosition, nextPostion) board -- verical movement
  in  Game {currentBoard = updatedBoard, playerPosition = nextPostion}
  -- the player position is being changed no matter the element moved
  -- this is not a problem when moving a box because move is called again to move the player

push :: Direction -> Game -> Game
push dir game =
  let pPosition      = playerPosition game
      boxPosition    = advance pPosition dir
      afterBoxPushed = move Box boxPosition dir game
  in  move Player pPosition dir afterBoxPushed

updateRow :: Element -> (Position, Position) -> Board -> Board
updateRow e (a@(r, c), b@(r', c')) board =
  let onGoal         = isGoalElement $ getElement a board
      toGoal         = isGoalElement $ getElement b board
      lineFrom       = board !! r
      lineTo         = board !! r'
      fElementOnGoal = if e == Player then PlayerOnGoal else BoxOnGoal
      newLineFrom    = updateAt c (if onGoal then Goal else Floor) lineFrom
      newLineTo      = updateAt c' (if toGoal then fElementOnGoal else e) lineTo
      newBoard       = updateAt r' newLineTo . updateAt r newLineFrom $ board
  in  newBoard

updateCol :: Element -> (Position, Position) -> Board -> Board
updateCol e (a@(r, c), b@(r', c')) board =
  let onGoal         = isGoalElement $ getElement a board
      toGoal         = isGoalElement $ getElement b board
      fElementOnGoal = if e == Player then PlayerOnGoal else BoxOnGoal
      oldLine        = board !! r
      newLine =
        updateAt c' (if toGoal then fElementOnGoal else e)
          . updateAt c (if onGoal then Goal else Floor)
          $ oldLine
      newBoard = updateAt r newLine board
  in  newBoard

updateAt :: Int -> a -> [a] -> [a]
updateAt = go 0
 where
  go :: Int -> Int -> a -> [a] -> [a]
  go _ _ _ []     = []
  go m n y (x:xs) = if m == n then y : xs else x : go (m + 1) n y xs
