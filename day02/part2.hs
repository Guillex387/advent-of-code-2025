import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShowId)
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

decomposeNumber :: Int -> [Int]
decomposeNumber num = take numLength $ map (`mod` 10) $ iterate (`div` 10) num
  where numLength = fromMaybe 0 $ elemIndex 0 $ iterate (`div` 10) num

zipN :: [Int] -> Int -> [[Int]]
zipN [] n = []
zipN list n = take n list : zipN (drop n list) n

checkPatternN :: Int -> Int -> Bool
checkPatternN number patternLen =
  let
    zippedNum = zipN (decomposeNumber number) patternLen
    pattern = head zippedNum
  in all (== pattern) zippedNum

checkInvalidId :: Int -> Bool
checkInvalidId idNumber =
  let
    digits = length $ decomposeNumber idNumber
  in any (checkPatternN idNumber) [1..digits-1] -- evaluates some invalid id from pattern 1 to digits

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
