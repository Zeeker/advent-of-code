import           Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  boxIds <- getBoxIds

  case boxIdsWithOneCharDiff boxIds of
    Nothing -> print "No matching box IDs found!"
    Just (id1, id2) ->
      let chars = filter (\char -> char `elem` id1) id2
       in print chars

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


boxIdsWithOneCharDiff :: [String] -> Maybe (String, String)
boxIdsWithOneCharDiff [] = Nothing
boxIdsWithOneCharDiff (boxId:boxIds) =
  case maybeCorrectIds boxId boxIds of
    Nothing  -> boxIdsWithOneCharDiff boxIds
    Just ids -> Just ids

maybeCorrectIds :: String -> [String] -> Maybe (String, String)
maybeCorrectIds id ids =
  case filter ((== 1) . (charDiff id)) ids of
    [id1, id2] -> Just (id1, id2)
    _          -> Nothing

charDiff :: String -> String -> Int
charDiff [] [] = 0
charDiff [] chars = length(chars)
charDiff chars [] = length(chars)
charDiff (char1:chars1) (char2:chars2)
  | char1 == char2 = 1 + diff
  | otherwise = diff
  where diff = charDiff chars1 chars2
