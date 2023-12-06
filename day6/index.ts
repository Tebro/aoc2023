import fs from "node:fs";
import * as R from "npm:ramda";

const fileContents = fs.readFileSync("input", { encoding: "utf8" });

const lines = fileContents
  .split("\n")
  .filter((l) => l.length > 3);

const timeLine = lines[0];
const distanceLine = lines[1];

const parseInts = R.map((x: string) => parseInt(x));

const [_t, ...timesRaw] = timeLine.split(/\s+/);
const times = parseInts(timesRaw);

const [_d, ...distancesRaw] = distanceLine.split(/\s+/);
const distances = parseInts(distancesRaw);

const races: number[][] = R.zip(times, distances);

const distancePerTime = (charge: number, total: number) =>
  (total - charge) * charge;

const part1 = races.map(([totalTime, record]) => {
  const winners = R.range(1, totalTime)
    .filter((x: number) => distancePerTime(x, totalTime) > record);
  return winners.length;
}).reduce((a, b) => a * b);

console.log("part1", part1);

const time = parseInt(timesRaw.join(""));
const distance = parseInt(distancesRaw.join(""));

const winners = R.range(1, time)
  .filter((x: number) => distancePerTime(x, time) > distance);
console.log("part2", winners.length);
