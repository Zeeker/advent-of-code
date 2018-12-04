import           Data.List

main :: IO ()
main = do
  boxIds <- getBoxIds

  let twoCharsCount = length $ filter (hasAtLeastDuplicateChars 2) boxIds
      threeCharsCount = length $ filter (hasAtLeastDuplicateChars 3) boxIds

  print $ (show twoCharsCount) ++ " * " ++ (show threeCharsCount) ++ " = " ++ (show $ twoCharsCount + threeCharsCount)

getBoxIds :: IO [String]
getBoxIds = do
  line <- getLine

  case parseBoxId line of
    Nothing    -> return []
    Just boxId -> do
      moreBoxIds <- getBoxIds
      return (boxId : moreBoxIds)

parseBoxId :: String -> Maybe String
parseBoxId []    = Nothing
parseBoxId boxId = Just boxId

hasAtLeastDuplicateChars :: Int -> [Char] -> Bool
hasAtLeastDuplicateChars 0 _ = True
hasAtLeastDuplicateChars _ [] = False
hasAtLeastDuplicateChars 1 [_] = True
hasAtLeastDuplicateChars _ [_] = False
hasAtLeastDuplicateChars count (char:chars) = hasAtLeastDuplicateChars' count 0 char $ sort chars

hasAtLeastDuplicateChars' :: Int -> Int -> Char -> [Char] -> Bool
hasAtLeastDuplicateChars' count current char (char':chars)
  | current >= count = True
  | char == char' = hasAtLeastDuplicateChars' count (current + 1) char chars
  | otherwise = hasAtLeastDuplicateChars' count 0 char' chars

hasAtLeastDuplicateChars' _ _ _ [] = False
