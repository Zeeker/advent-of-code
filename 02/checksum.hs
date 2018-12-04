import           Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  boxIds <- getBoxIds

  let twoCharsCount = length $ filter (hasExactlyDuplicateChars 2) boxIds
      threeCharsCount = length $ filter (hasExactlyDuplicateChars 3) boxIds

  putStrLn $ (show twoCharsCount) ++ " * " ++ (show threeCharsCount) ++ " = " ++ (show $ twoCharsCount * threeCharsCount)

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

hasExactlyDuplicateChars :: Int -> [Char] -> Bool
hasExactlyDuplicateChars 0 _         = True
hasExactlyDuplicateChars _ []        = False
hasExactlyDuplicateChars 1 [_]       = True
hasExactlyDuplicateChars _ [_]       = False
hasExactlyDuplicateChars count chars = count `elem` (countUniqueOccurences chars)

countUniqueOccurences :: [Char] -> [Int]
countUniqueOccurences chars =
  let countOccurences chars char = length . filter (== char) $ chars
      uniqueChars = Set.toList $ Set.fromList chars
   in map (countOccurences chars) uniqueChars
