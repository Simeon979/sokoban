module Sokoban.Internals.Types where

import Data.List (intersperse)

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

toGameElement :: Char -> Element
toGameElement '#' = Wall
toGameElement '@' = Player
toGameElement '+' = PlayerOnGoal
toGameElement '$' = Box
toGameElement '*' = BoxOnGoal
toGameElement '.' = Goal
toGameElement ' ' = Floor

type Line = [Element]

type Board = [Line]

data Game = Game { currentBoard :: Board
                 , playerPosition :: (Int, Int)
                 }

instance Show Game where
  show game = concat boardLines
    where
      -- ["##", "##"] -> ["##", "\n", "##"]
      boardLines = intersperse "\n" board'
      -- [[Wall, Wall]] -> [["#", "#"]] -> [["##"]]
      board' = concatMap show <$> currentBoard game

type Direction = String

type Position = (Int, Int)

data Terminals = Solved | Stopped | Restarted