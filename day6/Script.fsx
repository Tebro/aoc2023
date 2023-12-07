open System.IO
open System.Text.RegularExpressions

let lines = File.ReadAllLines("input")
let timesLine = lines.[0]
let distanceLine = lines.[1]

let parseInts xs = xs |> Seq.map (fun x -> int x)
let rx = Regex(@"\s+", RegexOptions.Compiled)
let times = rx.Split(timesLine) |> Seq.tail |> parseInts
let distances = rx.Split(distanceLine) |> Seq.tail |> parseInts

let races = Seq.zip times distances 

let distancePerTime charge total = (total - charge) * charge

let calcRaceNumWinners (totalTime, record) =
  seq {1 .. totalTime} |> Seq.filter (fun x -> (distancePerTime x totalTime) > record)

let part1 = races |> Seq.map calcRaceNumWinners |> Seq.map Seq.length |> Seq.reduce (fun a b -> a*b)

printf "part1 %d\n" part1
