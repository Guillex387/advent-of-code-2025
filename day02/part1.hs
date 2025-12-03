splitChar :: Char -> String -> [String]
splitChar delimiter s =
    case dropWhile (== delimiter) s of
        "" -> []
        rest ->
            let (chunk, remaining) = break (== delimiter) rest
            in chunk : splitChar delimiter remaining

data RangeId = RangeId Int Int deriving Show

parseRangeId :: String -> RangeId
parseRangeId input =
  let
    numbers = map read (splitChar '-' input)
    min = head numbers
    max = head $ tail numbers
  in RangeId min max

parseRangesId :: String -> [RangeId]
parseRangesId input = map parseRangeId (splitChar ',' input)

perfectSplit :: String -> (String, String)
perfectSplit word = splitAt middle word
  where middle = length word `div` 2

checkInvalidId :: Int -> Bool
checkInvalidId id =
  let
    (part1, part2) = perfectSplit $ show id
  in part1 == part2

listFromRange :: RangeId -> [Int]
listFromRange (RangeId min max) = [min..max]

sumInvalidIds :: [RangeId] -> Int
sumInvalidIds ranges = sum $ map (sum . filter checkInvalidId . listFromRange) ranges

main :: IO ()
main = do
  let inputFile = "input.txt"
  content <- readFile inputFile
  let ranges = parseRangesId content
  print $ sumInvalidIds ranges
