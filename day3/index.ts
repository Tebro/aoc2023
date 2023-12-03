import fs from "node:fs";

const fileContents = fs.readFileSync("input", { encoding: "utf8" });

interface NumPos {
  num: number;
  line: number;
  start: number;
  end: number;
}

interface SymPos {
  line: number;
  col: number;
  symbol: string;
}

const isDigit = (x: string) => !isNaN(Number(x));

const symbols: SymPos[] = [];
const numbers: NumPos[] = [];

fileContents
  .split("\n")
  .filter((l) => l.length > 3)
  .forEach((l, lineIdx) => {
    [...l].forEach((c, i) => {
      if (c === ".") {
        return;
      }
      if (isDigit(c)) {
        if (isDigit(l[i - 1])) return; // already handled
        let num = `${c}`;
        let j = i + 1;
        let end = -1;
        while (true) {
          if (isDigit(l[j])) {
            num += l[j];
            j += 1;
            continue;
          }
          end = j - 1;
          break;
        }
        numbers.push({
          num: parseInt(num),
          line: lineIdx,
          start: i,
          end,
        });
      }
      symbols.push({ line: lineIdx, col: i, symbol: c });
    });
  });

const nearby = (n: NumPos, s: SymPos): boolean => {
  if (s.line === n.line) {
    return s.col === n.start - 1 || s.col === n.end + 1;
  }
  if (s.line === n.line - 1 || s.line === n.line + 1) {
    return s.col >= n.start - 1 && s.col <= n.end + 1;
  }

  return false;
};

const sum1 = numbers
  .filter((n) => !!symbols.find((s) => nearby(n, s)))
  .reduce(
    (a, b) => a + b.num,
    0,
  );

console.log("part1", sum1);

const sum2 = symbols
  .filter((s) => s.symbol === "*")
  .map((s) => {
    const adjacentNums = numbers.filter((n) => nearby(n, s));
    if (adjacentNums.length !== 2) return 0;
    return adjacentNums[0].num * adjacentNums[1].num;
  }).reduce((a, b) => a + b, 0);

console.log("part2", sum2);
