parseInt x = read x :: Int

parseInts = map parseInt

distancePerTime charge total = (total - charge) * charge

calcRaceNumWinners (totalTime, record) =
  filter (\x -> distancePerTime x totalTime > record) [1 .. totalTime]

part1 :: [String] -> Int
part1 lines = do
  let timeLine = head lines
  let times = tail (words timeLine)
  let distanceLine = last lines
  let distances = tail (words distanceLine)
  let races = zip (parseInts times) (parseInts distances)
  let racesCalcedWinners = map calcRaceNumWinners races
  let racesNumWinners = map length racesCalcedWinners
  product racesNumWinners

part2 :: [String] -> Int
part2 lines =
  length (check [1 .. time])
  where
    timeLine = head lines
    times = tail (words timeLine)
    timesMerged = concat times
    time = parseInt timesMerged
    distanceLine = last lines
    distances = tail (words distanceLine)
    distancesMerged = concat distances
    distance = parseInt distancesMerged
    check = filter (\x -> distancePerTime x time > distance)

main :: IO ()
main = do
  input <- readFile "input"
  let ls = lines input
  print (part1 ls)
  print (part2 ls)
