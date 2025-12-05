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


-- Copy the array and change the positions with reachable rolls with a false
removeReachableRolls :: Array (Int, Int) Bool -> Array (Int, Int) Bool
removeReachableRolls grid = grid // map (\pos -> (pos, False)) (filter (isReachableRoll grid) $ filter (grid !) $ indices grid)

lastChange :: [Array (Int, Int) Bool] -> Array (Int, Int) Bool
lastChange (grid1:grid2:rest) =
  if elems grid1 == elems grid2 then
    grid1
  else
    lastChange (grid2:rest)

allPosibleRemoves :: Array (Int, Int) Bool -> Array (Int, Int) Bool
allPosibleRemoves grid = lastChange $ iterate removeReachableRolls grid

rollGridDiff :: [Bool] -> [Bool] -> Int
rollGridDiff [] [] = 0
rollGridDiff (head1:originalRest) (head2:reducedRest) =
  if head1 /= head2 then
    1 + rollGridDiff originalRest reducedRest
  else
    rollGridDiff originalRest reducedRest

main :: IO ()
main = do
  let inputFile = "input.txt"
  content <- readFile inputFile
  let rollsGrid = parseRollsGrid content
  let reducedGrid = allPosibleRemoves rollsGrid
  print $ rollGridDiff (elems rollsGrid) (elems reducedGrid)