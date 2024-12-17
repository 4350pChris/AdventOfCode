module Input where

parseInputFromFile :: String -> IO [String]
parseInputFromFile filename = do
  contents <- readFile filename
  return $ lines contents
