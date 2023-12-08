import fs from "node:fs";

const fileContents = fs.readFileSync("input", { encoding: "utf8" });

const lines = fileContents
  .split("\n")
  .filter(l => l.length > 1);

type ParsedLine = [string, [string, string]];

const parseMapLine = (line: string): ParsedLine => {
  const [key, targetsPart] = line.split(" = ");
  const [l, r] = targetsPart.split(", ");
  return [key, [l.substr(1), r.substr(0, 3)]];
};

const [instructions, ...mapLines] = lines;
const parsedMapLines = mapLines.map(parseMapLine);
const navSource = parsedMapLines.reduce(
  (a, [k, v]) => ({ ...a, [k]: v }),
  {} as { [k: string]: [string, string] },
);

const keys = Object.keys(navSource);
const startKeys = keys.filter(k => k[2] === "A");

const lengths = startKeys.map(k => {
  let upcomingInstructions = [...instructions]
  let current = navSource[k]
  let steps = 0;
  while (true) {
    const [instruction, ...rest] = upcomingInstructions;
    const idx = instruction === "L" ? 0 : 1;
    steps += 1;
    const nextKey = current[idx]
    if (nextKey[2] === "Z") {
      break
    }
    const next = navSource[nextKey];
    current = next
    upcomingInstructions = [...rest, instruction];
  }

  return steps;
});

// Thank you phind.com
function gcd(a: number, b: number): number {
   while (b !== 0) {
       const t = b;
       b = a % b;
       a = t;
   }
   return a;
}

function lcm(a: number, b: number): number {
   return Math.abs(a * b) / gcd(a, b);
}

const lcmv = lengths.reduce((a,b) => lcm(a,b));
console.log(lcmv);
