import Data.List (transpose, mapAccumL)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn isDelimiter list =
    case dropWhile isDelimiter list of
        [] -> []
        rest ->
            let (chunk, remaining) = break isDelimiter rest
            in chunk : splitOn isDelimiter remaining

dropLast :: [a] -> [a]
dropLast [] = []
dropLast l = reverse $ drop 1 $ reverse l

parseOperator :: String -> ([Int] -> Int)
parseOperator op =
  case op of
    "+" -> sum
    "*" -> product

parseOperations :: String -> [[Int] -> Int]
parseOperations input = map (\op -> parseOperator [op]) $ filter (/= ' ') $ last $ lines input

parseCephalopdNums :: String -> [[Int]]
parseCephalopdNums input =
  map (map read) $ splitOn (== "") $ map (filter (/= ' ')) $ transpose $ dropLast $ lines input

parseMathProblems :: String -> [([Int] -> Int, [Int])]
parseMathProblems input = zip (parseOperations input) (parseCephalopdNums input)

solveProblems :: [([Int] -> Int, [Int])] -> Int
solveProblems problems = sum $ map (\(f, nums) -> f nums) problems 

main :: IO ()
main = do
  let inputFile = "input.txt"
  content <- readFile inputFile
  let problems = parseMathProblems content
  print $ solveProblems problems