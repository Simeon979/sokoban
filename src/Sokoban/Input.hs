module Sokoban.Input (getValidPlayerInput) where


isValidInput :: String -> Bool
isValidInput i = i `elem` ["w", "s", "a", "d", "q", "r"]

getValidPlayerInput :: IO String
getValidPlayerInput = do
  input <- getLine
  if isValidInput input
    then pure input
    else putStrLn "invalid, please try again" >> getValidPlayerInput