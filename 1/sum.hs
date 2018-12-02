main :: IO ()
main = do
  numbers <- getNumbers
  putStrLn . show . sum $ numbers

getNumbers :: IO [Int]
getNumbers = do
  input <- getLine
  case parseInput input of
    Nothing -> return []
    Just anInt -> do
      moreInputs <- getNumbers
      return (anInt : moreInputs)

parseInput :: String -> Maybe Int
parseInput ('-':num) = Just $ -1 * (read num)
parseInput ('+':num) = Just $ read num
parseInput _ = Nothing
