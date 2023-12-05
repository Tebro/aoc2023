import fs from "node:fs";
import * as R from "npm:ramda";

const fileContents = fs.readFileSync("input", { encoding: "utf8" });

const lines = fileContents
  .split("\n")
  .filter((l) => l.length > 3);

const start1 = new Date();

const part1 = lines
  .map((l) => {
    const [winning, numbersRaw] = l.split(" | ");
    const [_title, winninNumsRaw] = winning.split(": ");
    const winningNums = winninNumsRaw.split(" ");
    const numbers = numbersRaw.split(" ");

    const score = numbers.reduce((a, b) => {
      const match = !!winningNums.find((wn) => wn === b);
      if (match) {
        if (a === 0) return 1;
        return a * 2;
      }
      return a;
    }, 0);
    return score;
  }).reduce((a, b) => a + b, 0);

const end1 = new Date();

console.log("part1", part1);
console.log("time1", end1.getTime() - start1.getTime());

const start2 = new Date();

interface ScratchCard {
  id: number;
  winners: number[];
  numbers: number[];
}

const cards: ScratchCard[] = lines.map((l) => {
  const [winning, numbersRaw] = l.split(" | ");
  const [title, winninNumsRaw] = winning.split(": ");
  const [_, ...rest] = title.split(" ");
  const id = rest[rest.length - 1];
  const winners = winninNumsRaw.split(" ").filter((n) => n !== "").map((n) =>
    parseInt(n)
  );
  const numbers = numbersRaw.split(" ").filter((n) => n !== "").map((n) =>
    parseInt(n)
  );

  return {
    id: parseInt(id),
    numbers,
    winners,
  };
});

//const cardsMap = cards.reduce((a, b) => ({
//  ...a,
//  [b.id]: b,
//}), {} as { [key: number]: ScratchCard });

const cardsMap: {[key: number]: ScratchCard} = R.indexBy(R.prop('id'), cards);

for (let i = 0; i < cards.length; i++) {
  const matches = cards[i].numbers.filter((n) =>
    !!cards[i].winners.find((w) => w === n)
  );
  const currentId = cards[i].id;
  matches.forEach((_, j) => {
    cards.push(cardsMap[`${currentId + 1 + j}`]);
  });
}

const end2 = new Date();

console.log("part2", cards.length);
console.log("time2", end2.getTime() - start2.getTime());
