module Input where

parseInputForDay :: String -> Bool -> IO [String]
parseInputForDay day test = do
  let filename = if test then day ++ "_test" else day
  contents <- readFile ("inputs/" ++ filename ++ ".txt")
  return $ lines contents
