import Debug.Trace (traceShowId)

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

rotate :: Int -> Rotation -> Int
rotate value (Rotation dir amount) =
  case dir of
    Lf -> let
        signResult = (value - amount) `mod` 100
      in if signResult < 0 then 100 + signResult else signResult
    Rt -> (value + amount) `mod` 100

countZeros :: Int -> [Rotation] -> Int
countZeros value [] = if value == 0 then 1 else 0
countZeros value (rotation:rotations) =
  let
    nextValue = rotate value rotation
    isZero = if nextValue == 0 then 1 else 0
  in isZero + countZeros nextValue rotations

main :: IO ()
main = do
  let inputFile = "input.txt"
  content <- readFile inputFile
  let rotations = parseRotations content
  print $ countZeros 50 rotations