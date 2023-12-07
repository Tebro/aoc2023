open System.IO
open System.Text.RegularExpressions

let lines = File.ReadAllLines("input")
let timesLine = lines.[0]
let distanceLine = lines.[1]

let parseInts xs = xs |> Seq.map (fun x -> int64 x)
let rx = Regex(@"\s+", RegexOptions.Compiled)
let times = rx.Split(timesLine) |> Seq.tail |> parseInts
let distances = rx.Split(distanceLine) |> Seq.tail |> parseInts

let races = Seq.zip times distances

let distancePerTime (charge: int64) (total: int64) = (total - charge) * charge

let calcRaceNumWinners ((totalTime, record): int64*int64) =
    seq { 1.. int totalTime }
    |> Seq.filter (fun x -> (distancePerTime x totalTime) > record)

let part1 =
    races
    |> Seq.map calcRaceNumWinners
    |> Seq.map Seq.length
    |> Seq.reduce (fun a b -> a * b)

printf "part1 %d\n" part1

let time = rx.Split(timesLine) |> Seq.tail |> String.concat "" |> int64
let distance = rx.Split(distanceLine) |> Seq.tail |> String.concat "" |> int64

let winners =
    seq { 1.. int time } |> Seq.filter (fun x -> (distancePerTime x time) > distance)

printf "part2 %d\n" (Seq.length winners)
