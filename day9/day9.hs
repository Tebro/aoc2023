import Debug.Trace (traceShow)

-- nice to have tool
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

calcNext2 :: Int -> [[Int]] -> Int
calcNext2 step [] = step
calcNext2 step (x:xs) = calcNext2 nextStep xs
  where
    nextStep = head x - step

bothParts :: [String] -> (Int, Int)
bothParts lines = (sum nextsP1, sum nextsP2)
  where
    vals = map (parseInts . words) lines
    expanded = map (\x -> calcLine [x] x) vals
    rev = map reverse expanded
    nextsP1 = map (calcNext 0) rev
    nextsP2 = map (calcNext2 0) rev


main :: IO ()
main = do
  input <- readFile "input"
  let ls = lines input
  let (p1, p2) = bothParts ls
  print p1
  print p2
