import Data.List (singleton, sort, sortBy, elemIndex, maximumBy, delete, subsequences)
import Data.Ord (Down(Down), comparing)
import Debug.Trace (traceShowId)
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.Int (Int64)

parseBatteryBanks :: String -> [[Int64]]
parseBatteryBanks banks = map (map (read . singleton)) $ lines banks

subseqBankToJoltage :: [Int64] -> Int64
subseqBankToJoltage sub = read $ concatMap show sub

subseqComparison :: [Int64] -> [Int64] -> Ordering
subseqComparison [] [] = EQ
subseqComparison bank [] = GT
subseqComparison [] bank2 = LT
subseqComparison (a:bank) (b:bank2)
  | a == b = subseqComparison bank bank2
  | a < b = LT
  | a > b = GT

maxSubseqN :: Int -> [Int64] -> [Int64]
maxSubseqN 1 sub = singleton $ maximum sub 
maxSubseqN n sub =
  let
    maxDigit = head $ maximumBy subseqComparison $ takeWhile (\s -> length s >= n) $ iterate tail sub
    (_, rest) = break (== maxDigit) sub
  in maxDigit : maxSubseqN (n-1) (tail rest)

joltage12 :: [Int64] -> Int64
joltage12 bank = subseqBankToJoltage $ maxSubseqN 12 bank

totalJoltage12 :: [[Int64]] -> Int64
totalJoltage12 = sum . map joltage12

main :: IO ()
main = do
  let inputFile = "input.txt"
  content <- readFile inputFile
  let banks = parseBatteryBanks content
  print $ totalJoltage12 banks