module Sokoban.Parse 
  ( parseBoards
  ) where

import Data.List.Split        (endBy)

import Sokoban.Internals.Types

-- | Given the list of lines in a properly formatted file, parse the file
parseBoards :: [String] -> [Board]
parseBoards = (:) <$> parseOne <*> parseRest

parseOne :: [String] -> Board
parseOne = fmap (fmap toGameElement)
         . endBy "\r"
         . concat
         . takeWhile (/= "\r")
         . drop 1

parseRest :: [String] -> [Board]
parseRest left | null left = []
               | otherwise = parseBoards . tail . dropWhile (/= "\r") $ left