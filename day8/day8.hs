import Data.Map qualified as Map
import Debug.Trace (traceShow)

split :: String -> (String, (String, String))
split [a1, a2, a3, _, _, _, _, b1, b2, b3, _, _, c1, c2, c3, _] =
  ([a1, a2, a3] :: String, ([b1, b2, b3] :: String, [c1, c2, c3] :: String))

type Checker = String -> Bool

navigate :: Int -> Checker -> Map.Map String (String, String) -> (String, String) -> String -> Int
navigate acc checker navigationSource current (x : xs)
  | checker next = acc + 1
  | otherwise = navigate (acc + 1) checker navigationSource (Map.findWithDefault ("foo", "foo") next navigationSource) xs
  where
    lr = if x == 'L' then fst else snd
    next = lr current

lookup' :: String -> Map.Map String (String, String) -> (String, String)
lookup' = Map.findWithDefault ("", "")

part1 :: [String] -> Int
part1 lines = do
  let instructions = cycle (head lines)
  let mapLines = drop 2 lines
  let navigationSource = Map.fromList (map split mapLines)
  let start = lookup' "AAA" navigationSource
  let checker x = x == "ZZZ"
  navigate 0 checker navigationSource start instructions

endsIn :: Char -> String -> Bool
endsIn x [_, _, y] = x == y

part2 :: [String] -> Int
part2 lines = do
  let instructions = cycle (head lines)
  let mapLines = drop 2 lines
  let navigationSource = Map.fromList (map split mapLines)
  let keys = Map.keys navigationSource
  let startKeys = filter (endsIn 'A') keys
  let starts = map (`lookup'` navigationSource) startKeys
  let checker = endsIn 'Z'
  let lengths = map (\x -> navigate 0 checker navigationSource x instructions) starts
  foldl1 lcm lengths


main :: IO ()
main = do
  input <- readFile "input"
  let ls = lines input
  print (part1 ls)
  print (part2 ls)
