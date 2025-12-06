import Debug.Trace (traceShowId)
import Data.Int (Int64)

mapSecond :: (b -> y) -> (a, b) -> (a, y)
mapSecond transform (a, b) = (a, transform b)

splitChar :: Char -> String -> [String]
splitChar delimiter s =
    case dropWhile (== delimiter) s of
        "" -> []
        rest ->
            let (chunk, remaining) = break (== delimiter) rest
            in chunk : splitChar delimiter remaining

data FreshRangeId = FreshRangeId Int64 Int64 deriving Show

parseRange :: String -> FreshRangeId
parseRange input =
  let
    numbers = map read (splitChar '-' input)
    min = head numbers
    max = head $ tail numbers
  in FreshRangeId min max

parseInstanceStr :: String -> ([FreshRangeId], [Int64])
parseInstanceStr input =
  let
    (rangesStr, idsStr) = mapSecond tail $ break (== "") $ lines input
    ranges = map parseRange rangesStr
    ids = map read idsStr
  in (ranges, ids)

rangeLength :: FreshRangeId -> Int64
rangeLength (FreshRangeId min max) = max - min + 1

areSolapated :: FreshRangeId -> FreshRangeId -> Bool
areSolapated (FreshRangeId min max) (FreshRangeId min' max')
  | max < min' || max' < min = False
  | otherwise = True

rangeUnion :: FreshRangeId -> FreshRangeId -> FreshRangeId
rangeUnion (FreshRangeId min max) (FreshRangeId min' max') =
  FreshRangeId (minimum [min, min']) (maximum [max, max'])

addRangeToLargeUnion :: FreshRangeId -> [FreshRangeId] -> [FreshRangeId]
addRangeToLargeUnion r [] = [r]
addRangeToLargeUnion r (r':rest) =
  if areSolapated r r' then
    addRangeToLargeUnion (rangeUnion r r') rest
  else
    r' : addRangeToLargeUnion r rest

rangesToUniqueRanges :: [FreshRangeId] -> [FreshRangeId]
rangesToUniqueRanges = foldr addRangeToLargeUnion []

totalUniqueFreshIds :: [FreshRangeId] -> Int64
totalUniqueFreshIds ranges = sum $ map rangeLength $ rangesToUniqueRanges ranges
  

main :: IO ()
main = do
  let inputFile = "input.txt"
  content <- readFile inputFile
  let (ranges, ids) = parseInstanceStr content
  print $ totalUniqueFreshIds ranges