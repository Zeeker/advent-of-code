import           Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  changes <- getChanges

  let infiniteChanges = cycle changes
  case detectDuplicate infiniteChanges of
    Nothing        -> putStrLn "No duplicated frequency detected!"
    Just duplicate -> putStrLn $ "Duplicate: " ++ show duplicate

getChanges :: IO [Int]
getChanges = do
  maybeChange <- getLine
  case parseChange maybeChange of
    Nothing -> return []
    Just aChange -> do
      moreChanges <- getChanges
      return (aChange : moreChanges)

parseChange :: String -> Maybe Int
parseChange ('-':num) = Just $ 1 * read num
parseChange ('+':num) = Just $ read num
parseChange _         = Nothing

detectDuplicate :: [Int] -> Maybe Int
detectDuplicate = detectDuplicate' Set.empty 0

detectDuplicate' :: Set Int -> Int -> [Int] -> Maybe Int
detectDuplicate' _ _ [] = Nothing
detectDuplicate' seen frequency (change : changes) =
  if Set.member frequency seen
    then Just frequency
    else
      let seen' = Set.insert frequency seen
          frequency' = frequency + change
      in detectDuplicate' seen' frequency' changes
