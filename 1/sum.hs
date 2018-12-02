main :: IO ()
main = do
  changes <- getChanges
  putStrLn . show . sum $ changes

getChanges :: IO [Int]
getChanges = do
  maybeChange <- getLine
  case parseChange maybeChange of
    Nothing -> return []
    Just aChange -> do
      moreChanges <- getChanges
      return (aChange : moreChanges)

parseChange :: String -> Maybe Int
parseChange ('-':num) = Just $ -1 * (read num)
parseChange ('+':num) = Just $ read num
parseChange _ = Nothing
