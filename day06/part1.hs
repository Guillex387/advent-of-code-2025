import Data.List (transpose, mapAccumL)
splitChar :: Char -> String -> [String]
splitChar delimiter s =
    case dropWhile (== delimiter) s of
        "" -> []
        rest ->
            let (chunk, remaining) = break (== delimiter) rest
            in chunk : splitChar delimiter remaining

parseOperator :: String -> ([Int] -> Int)
parseOperator op =
  case op of
    "+" -> sum
    "*" -> product

parseMathProblems :: String -> [([Int] -> Int, [Int])]
parseMathProblems input =
  let
    strProblemList = transpose $ map (splitChar ' ') $ lines input
  in map (\p -> (parseOperator $ last p, map read $ drop 1 $ reverse p)) strProblemList

solveProblems :: [([Int] -> Int, [Int])] -> Int
solveProblems problems = sum $ map (\(f, nums) -> f nums) problems 

main :: IO ()
main = do
  let inputFile = "input.txt"
  content <- readFile inputFile
  let problems = parseMathProblems content
  print $ solveProblems problems