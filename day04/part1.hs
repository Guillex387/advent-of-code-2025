import Data.Array

parseRollsGrid :: String -> Array (Int, Int) Bool
parseRollsGrid input =
  let
    rowsStr = lines input
    rowCount = length rowsStr
    columnCount = length $ head rowsStr
    rawMatrix = map (map (== '@')) rowsStr
  in array ((0, 0), (rowCount-1, columnCount-1))
    $ concatMap (\(r, rowList) -> zip (map (\c -> (r, c)) [0..columnCount-1]) rowList) (zip [0..rowCount-1] rawMatrix)

notOutOfBounds :: Array (Int, Int) Bool -> (Int, Int) -> Bool
notOutOfBounds grid (row, column) 
  | row < 0 || column < 0 = False
  | let (_, (limitRow, limitColumn)) = bounds grid
    in row > limitRow || column > limitColumn = False
  | otherwise = True

applyTransformation :: (Int, Int) -> (Int, Int) -> (Int, Int)
applyTransformation pos transformation =
  let
    (t1, t2) = transformation
    (p1, p2) = pos
  in (t1 + p1, t2 + p2)

neighborsIndices :: (Int, Int) -> [(Int, Int)]
neighborsIndices pos = map (applyTransformation pos) [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

isReachableRoll :: Array (Int, Int) Bool -> (Int, Int) -> Bool
isReachableRoll grid pos =
  let
    rollsNeighborsCount = length $ filter id $ map (grid !) $ filter (notOutOfBounds grid) $ neighborsIndices pos
  in rollsNeighborsCount < 4

countReachableRolls :: Array (Int, Int) Bool -> Int
countReachableRolls grid = length $ filter id $ map (isReachableRoll grid) $ filter (grid !) $ indices grid

main :: IO ()
main = do
  let inputFile = "input.txt"
  content <- readFile inputFile
  let rollsGrid = parseRollsGrid content
  print $ countReachableRolls rollsGrid