import Data.List (singleton, sort, sortBy, elemIndex)
import Data.Ord (Down(Down), comparing)
import Debug.Trace (traceShowId)
import Data.Maybe (fromMaybe)

parseBatteryBanks :: String -> [[Int]]
parseBatteryBanks banks = map (map (read . singleton)) $ lines banks

joltage :: [Int] -> Int
joltage bank =
  let
    maxElem = maximum $ tail $ reverse bank
    rest = tail $ dropWhile (< maxElem) bank
    digit2 = maximum rest
  in maxElem * 10 + digit2

totalJoltage :: [[Int]] -> Int
totalJoltage = sum . map joltage

main :: IO ()
main = do
  let inputFile = "input.txt"
  content <- readFile inputFile
  let banks = parseBatteryBanks content
  print $ totalJoltage banks