import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Debug.Trace

main :: IO ()
main = do
  boxIds <- getBoxIds

  putStrLn
    . maybe "No matching box IDs found!" boxIdsMessage
    . boxIdsWithOneCharDiff
    $ boxIds


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
  case filter ((==1) . charDiff id) ids of
    []    -> Nothing
    [id2] -> Just (id, id2)
    list  -> trace ("Unexpected filter result: " ++ (show list)) Nothing


charDiff :: String -> String -> Int
charDiff [] [] = 0
charDiff [] chars = length chars
charDiff chars [] = length chars
charDiff (char1:chars1) (char2:chars2)
  | char1 /= char2 = 1 + diff
  | otherwise = diff
  where diff = charDiff chars1 chars2


boxIdsMessage :: (String, String) -> String
boxIdsMessage (id1, id2) =
  let commonChars = determineCommonChars id1 id2
    in "Correct IDs\n" ++
      "  " ++ id1 ++ "\n" ++
      "  " ++ id2 ++ "\n" ++
      "Common chars: " ++ commonChars

determineCommonChars :: String -> String -> String
determineCommonChars chars1 chars2 =
  let uniqueChars1 = Set.fromList chars1
      uniqueChars2 = Set.fromList chars2
      differenceChars  = Set.difference uniqueChars1 uniqueChars2
   in filter (\c -> not $ c `elem` differenceChars) chars1
