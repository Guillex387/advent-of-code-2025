splitChar :: Char -> String -> [String]
splitChar delimiter s =
    case dropWhile (== delimiter) s of
        "" -> []
        rest ->
            let (chunk, remaining) = break (== delimiter) rest
            in chunk : splitChar delimiter remaining

data RotationDir = Lf | Rt deriving Show
data Rotation = Rotation RotationDir Int deriving Show

parseRotation :: String -> Rotation
parseRotation line =
  let
    directionChar = head line
    direction = if directionChar == 'L' then Lf else Rt
    amount = read $ tail line :: Int
  in
    Rotation direction amount

parseRotations :: String -> [Rotation]
parseRotations lines = map parseRotation $ splitChar '\n' lines

unitaryRotation :: Int -> RotationDir -> Int
unitaryRotation 0 Lf = 99
unitaryRotation 99 Rt = 0
unitaryRotation value dir =
  case dir of
    Lf -> value - 1
    Rt -> value + 1

generateOptions :: Int -> Rotation -> [Int]
generateOptions init (Rotation dir amount) =
  if amount == 0 then
    []
  else
    let
      nextNumber = unitaryRotation init dir
    in nextNumber : generateOptions nextNumber (Rotation dir (amount - 1))

countZeros :: Int -> [Rotation] -> Int
countZeros value [] = 0
countZeros value (rotation:rotations) =
  let
    passedValues = generateOptions value rotation
    zeroCount = length $ filter (== 0) passedValues
  in zeroCount + countZeros (last passedValues) rotations

main :: IO ()
main = do
  let inputFile = "input.txt"
  content <- readFile inputFile
  let rotations = parseRotations content
  print $ countZeros 50 rotations