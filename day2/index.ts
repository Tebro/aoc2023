import fs from 'node:fs';

const fileContents = fs.readFileSync('input', {encoding: "utf8"});

const LIMIT_RED = 12;
const LIMIT_GREEN = 13;
const LIMIT_BLUE = 14;

const parseLinePart1 = (line: string) => {
  const [idPart, dataPart] = line.split(": ");
  const id = idPart.split(" ")[1];
  const sets = dataPart.split("; ");

  const max: {[k: string]: number} = {
    blue: 0,
    red: 0,
    green: 0
  };

  sets.forEach(s => {
    const parts = s.split(", ");
    parts.forEach(p => {
      const [v, k] = p.split(" ");
      max[k] = Math.max(max[k], parseInt(v));
    });
  });

  if (max.green > LIMIT_GREEN || max.red > LIMIT_RED || max.blue > LIMIT_BLUE) {
    return 0;
  }
  return parseInt(id);
};


const part1 = fileContents
  .split("\n")
  .filter(l => l.length > 3)
  .map(parseLinePart1)
  .reduce((a,b) => a+b, 0);

console.log("part1", part1);

const parseLinePart2 = (line: string) => {
  const [_idPart, dataPart] = line.split(": ");
  const sets = dataPart.split("; ");

  const max: {[k: string]: number} = {
    blue: 0,
    red: 0,
    green: 0
  };

  sets.forEach(s => {
    const parts = s.split(", ");
    parts.forEach(p => {
      const [v, k] = p.split(" ");
      max[k] = Math.max(max[k], parseInt(v));
    });
  });

  return max.red*max.blue*max.green;
};

const part2 = fileContents
  .split("\n")
  .filter(l => l.length > 3)
  .map(parseLinePart2)
  .reduce((a,b) => a+b, 0);

console.log("part2", part2);
