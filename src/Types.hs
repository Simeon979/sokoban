module Types
  ( Element (..)
  , Line
  , Board
  , Game (..)
  , Input
  , Position
  , toGameElement
  ) where

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


-- | each line on the board is a list of elements i.e
{-  
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

type Input = String

type Position = (Int, Int)