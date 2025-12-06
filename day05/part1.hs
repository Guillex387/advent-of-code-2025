mapSecond :: (b -> y) -> (a, b) -> (a, y)
mapSecond transform (a, b) = (a, transform b)

splitChar :: Char -> String -> [String]
splitChar delimiter s =
    case dropWhile (== delimiter) s of
        "" -> []
        rest ->
            let (chunk, remaining) = break (== delimiter) rest
            in chunk : splitChar delimiter remaining

data FreshRangeId = FreshRangeId Int Int deriving Show

parseRange :: String -> FreshRangeId
parseRange input =
  let
    numbers = map read (splitChar '-' input)
    min = head numbers
    max = head $ tail numbers
  in FreshRangeId min max

parseInstanceStr :: String -> ([FreshRangeId], [Int])
parseInstanceStr input =
  let
    (rangesStr, idsStr) = mapSecond tail $ break (== "") $ lines input
    ranges = map parseRange rangesStr
    ids = map read idsStr
  in (ranges, ids)

isFreshId :: [FreshRangeId] -> Int -> Bool
isFreshId ranges id' = any (\(FreshRangeId min max) -> id' >= min && id' <= max) ranges

countFreshId :: [FreshRangeId] -> [Int] -> Int
countFreshId ranges ids = length $ filter (isFreshId ranges) ids

main :: IO ()
main = do
  let inputFile = "input.txt"
  content <- readFile inputFile
  let (ranges, ids) = parseInstanceStr content
  print $ countFreshId ranges ids