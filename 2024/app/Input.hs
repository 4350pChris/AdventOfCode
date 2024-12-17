module Input where

parseInputForDay :: String -> IO [String]
parseInputForDay day = do
  contents <- readFile ("inputs/" ++ day ++ ".txt")
  return $ lines contents
