import Debug.Trace (traceShow)

dbg x = traceShow x x

parseInt x = read x :: Int
parseInts = map parseInt

lineDiff :: [Int] -> [Int] -> [Int]
lineDiff acc (x:xs)
  | null xs = acc
  | otherwise = lineDiff (acc ++ [head xs - x]) xs

calcLine :: [[Int]] -> [Int] -> [[Int]]
calcLine acc line
  | all (== 0) diff = acc ++ [diff]
  | otherwise = calcLine (acc ++ [diff]) diff
  where
   diff = lineDiff [] line

calcNext :: Int -> [[Int]] -> Int
calcNext step [] = step
calcNext step (x:xs) = calcNext nextStep xs
  where
    nextStep = last x + step

part1 :: [String] -> Int
part1 lines = sum nexts
  where
    vals = map (parseInts . words) lines
    expanded = map (\x -> calcLine [x] x) vals
    rev = map reverse expanded
    nexts = map (calcNext 0 ) rev

calcNext2 :: Int -> [[Int]] -> Int
calcNext2 step [] = step
calcNext2 step (x:xs) = calcNext2 nextStep xs
  where
    nextStep = head x - step

part2 :: [String] -> Int
part2 lines = sum nexts
  where
    vals = map (parseInts . words) lines
    expanded = map (\x -> calcLine [x] x) vals
    rev = map reverse expanded
    nexts = map (calcNext2 0) rev

main :: IO ()
main = do
  input <- readFile "input"
  let ls = lines input
  print (part1 ls)
  print (part2 ls)
