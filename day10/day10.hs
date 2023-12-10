import Data.List (find)
import Data.Maybe (isJust)
import Debug.Trace (traceShow)

-- nice to have tool
dbg x = traceShow x x

data Direction = N | E | S | W
  deriving (Show)

type PipeMoves = (Direction, Direction)

moves :: Char -> PipeMoves
moves '|' = (N, S)
moves '-' = (W, E)
moves 'L' = (N, E)
moves 'J' = (W, N)
moves '7' = (W, S)
moves 'F' = (S, E)

type Coord = (Int, Int)

go :: Coord -> Direction -> Coord
go (x, y) N = (x, y - 1)
go (x, y) S = (x, y + 1)
go (x, y) W = (x - 1, y)
go (x, y) E = (x + 1, y)

dirTo :: Coord -> Coord -> Direction
dirTo (fromX, fromY) (toX, toY)
  | fromY == toY && fromX > toX = W
  | fromY == toY && fromX < toX = E
  | fromX == toX && fromY > toY = N
  | fromX == toX && fromY < toY = S

dirThrough :: Char -> Direction -> Direction
dirThrough '|' N = N
dirThrough '|' S = S
dirThrough '-' E = E
dirThrough '-' W = W
dirThrough 'L' S = E
dirThrough 'L' W = N
dirThrough 'J' E = N
dirThrough 'J' S = W
dirThrough '7' E = S
dirThrough '7' N = W
dirThrough 'F' W = S
dirThrough 'F' N = E

findStart :: Int -> [String] -> Coord
findStart y (l : ls)
  | x /= -1 = (x, y)
  | otherwise = findStart (y + 1) ls
  where
    findStartX x (c : cs)
      | c == 'S' = x
      | null cs = -1
      | otherwise = findStartX (x + 1) cs
    x = findStartX 0 l

coordsAround :: Coord -> [Coord]
coordsAround (x, y) =
  [ (x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    (x - 1, y),
    (x + 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  ]

type Pipe = (Coord, PipeMoves, Char)

type NavMap = [String]

pipeConnections :: NavMap -> Pipe -> [Coord]
pipeConnections navMap (c, ms, _) =
  [go c (fst ms), go c (snd ms)]

isValid :: Coord -> Bool
isValid (x,y) = x >= 0 && y >= 0

getPipe :: NavMap -> Coord -> Pipe
getPipe navMap c@(x,y) = (c, moves ch, ch)
  where
    ch = navMap !! y !! x

travel :: [Pipe] -> NavMap -> Pipe -> Direction -> [Pipe]
travel acc navMap p@(pCoord, _, pChar) inDir
  | pChar == 'S' = acc
  | otherwise = travel (acc ++ [p]) navMap nextPipe outDir
  where
    outDir = dirThrough pChar inDir
    nextCoord = go pCoord outDir
    nextPipe = getPipe navMap nextCoord


part1 :: [String] -> Int
part1 navMap =
  length loop `div` 2 + 1
  where
    startCoord = findStart 0 navMap
    around = filter isValid (coordsAround startCoord)
    aroundSymbols = map (getPipe navMap) around
    pipes = filter (\(_, _, x) -> x /= '.') aroundSymbols
    connectedToStart pipe =
      isJust (find (== startCoord) (pipeConnections navMap pipe))
    connected = filter connectedToStart pipes
    firstPipe@(firstCoord, _, _) = head connected
    startDir = dirTo startCoord firstCoord
    loop = travel [] navMap firstPipe startDir


main :: IO ()
main = do
  input <- readFile "input"
  let ls = lines input
  print (part1 ls)

-- print p2
