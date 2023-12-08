import Data.Map qualified as Map
import Debug.Trace (traceShow)

split :: String -> (String, (String, String))
split [a1, a2, a3, _, _, _, _, b1, b2, b3, _, _, c1, c2, c3, _] =
  ([a1, a2, a3] :: String, ([b1, b2, b3] :: String, [c1, c2, c3] :: String))

navigate :: Int -> Map.Map String (String, String) -> (String, String) -> String -> Int
navigate acc navigationSource current (x : xs)
  | next == "ZZZ" = acc + 1
  | otherwise = navigate (acc + 1) navigationSource (Map.findWithDefault ("foo", "foo") next navigationSource) xs
  where
    lr = if x == 'L' then fst else snd
    next = lr current

part1 :: [String] -> Int
part1 lines = do
  let instructions = cycle (head lines)
  let mapLines = drop 2 lines
  let navigationSource = Map.fromList (map split mapLines)
  let (Just start) = Map.lookup "AAA" navigationSource
  navigate 0 navigationSource start instructions

endsIn :: Char -> String -> Bool
endsIn x [_, _, y] = x == y

navigate2 :: Int -> Map.Map String (String, String) -> [(String, String)] -> String -> Int
navigate2 acc navigationSource currents (x : xs)
  | allAtEnd = acc
  | otherwise = navigate2 (acc + 1) navigationSource nexts xs
  where
    lr = if x == 'L' then fst else snd
    nextKeys = map lr currents
    allAtEnd = all (endsIn 'Z') nextKeys
    nexts = map ( \x -> Map.findWithDefault ("foo", "foo") x navigationSource) nextKeys

-- this is just too slow, will not get there in a sane amount of time
-- solved with part2.ts instead for now... TODO: make a haskell version
part2 :: [String] -> Int
part2 lines = do
  let instructions = cycle (head lines)
  let mapLines = drop 2 lines
  let navigationSource = Map.fromList (map split mapLines)
  let keys = Map.keys navigationSource
  let startKeys = filter (endsIn 'A') keys
  let starts = map (\x -> Map.findWithDefault ("Foo", "Foo") x navigationSource) startKeys
  navigate2 1 navigationSource starts instructions

main :: IO ()
main = do
  input <- readFile "input"
  let ls = lines input
  print (part1 ls)
