import fs from "node:fs";
import * as R from "npm:ramda";

const fileContents = fs.readFileSync("input", { encoding: "utf8" });

const lines = fileContents
  .split("\n")
  .filter((l) => l.length > 3);

type RawLine = [string, number];

const rawLines: RawLine[] = lines.map((l) => {
  const [hand, betRaw] = l.split(" ");
  return [hand, parseInt(betRaw)];
});

//const potato = R.groupBy((x: RawLine) => x[0], rawLines)
//Object.keys(potato).forEach(k => {
//  if (potato[k].length > 1) {
//    console.log(k, potato[k].length);
//  }
//});

enum HandType {
  HighCard = 1,
  OnePair,
  TwoPair,
  ThreeOfAKind,
  FullHouse,
  FourOfAKind,
  FiveOfAKind,
}

const parseHandType = (input: RawLine): HandType => {
  const grouped = R.groupBy(R.identity, input[0].split(""));

  let numPairs = 0;
  let threeOfAKind = false;

  const keys = Object.keys(grouped);
  for (const key of keys) {
    const size = grouped[key].length;
    switch (size) {
      case 5:
        return HandType.FiveOfAKind;
      case 4:
        return HandType.FourOfAKind;
      case 3:
        threeOfAKind = true;
        break;
      case 2:
        numPairs += 1;
        break;
    }
  }

  if (threeOfAKind) {
    return numPairs === 1 ? HandType.FullHouse : HandType.ThreeOfAKind;
  }

  switch (numPairs) {
    case 2:
      return HandType.TwoPair;
    case 1:
      return HandType.OnePair;
  }

  return HandType.HighCard;
};

const cardOrder = "23456789TJQKA";

const handTypes = rawLines.map(parseHandType);
type RawLineWithHandtype = [string, number, HandType];
const combined: RawLineWithHandtype[] = R.map(
  R.flatten,
  R.zip(rawLines, handTypes),
);

const comparer =
  (cardOrder: string) => (a: RawLineWithHandtype, b: RawLineWithHandtype) => {
    if (a[2] !== b[2]) {
      return a[2] - b[2];
    }
    for (let i = 0; i < a[0].length; i++) {
      if (a[0][i] !== b[0][i]) {
        const aIdx = cardOrder.indexOf(a[0][i]);
        const bIdx = cardOrder.indexOf(b[0][i]);
        return aIdx - bIdx;
      }
    }
    console.log("same position?!", a, b);
    return 0;
  };

const sorted: RawLineWithHandtype[] = R.sort(comparer(cardOrder), combined);
const values = sorted.map((h, i) => h[1] * (i + 1));
//console.log(R.reverse(R.zip(sorted, values)));

const winnings = R.sum(values);
console.log("part1", winnings);

const parseHandType2 = (input: RawLine): HandType => {
  const grouped = R.groupBy(R.identity, input[0].split(""));

  let numPairs = 0;
  let threeOfAKind = false;

  delete grouped["J"];

  const keys = Object.keys(grouped);
  for (const key of keys) {
    const size = grouped[key].length;
    switch (size) {
      case 5:
        return HandType.FiveOfAKind;
      case 4:
        return HandType.FourOfAKind;
      case 3:
        threeOfAKind = true;
        break;
      case 2:
        numPairs += 1;
        break;
    }
  }

  if (threeOfAKind) {
    return numPairs === 1 ? HandType.FullHouse : HandType.ThreeOfAKind;
  }

  switch (numPairs) {
    case 2:
      return HandType.TwoPair;
    case 1:
      return HandType.OnePair;
  }

  return HandType.HighCard;
};

const resolveJokers = (input: RawLineWithHandtype): RawLineWithHandtype => {
  const [hand, bet, originalHandType] = input;
  const grouped = R.groupBy(R.identity, hand.split(""));

  if (grouped["J"]) {
    let numJokers = grouped["J"].length;
    let newType = originalHandType;
    while (numJokers > 0) {
      switch (newType) {
        case HandType.FourOfAKind:
          newType = HandType.FiveOfAKind;
          break;
        case HandType.ThreeOfAKind:
          newType = HandType.FourOfAKind;
          break;
        case HandType.TwoPair:
          newType = HandType.FullHouse;
          break;
        case HandType.OnePair:
          newType = HandType.ThreeOfAKind;
          break;
        case HandType.HighCard:
          newType = HandType.OnePair;
          break;
      }
      numJokers -= 1;
    }
    return [hand, bet, newType];
  }
  return input;
};

const cardOrder2 = "J23456789TQKA";
const handTypes2 = rawLines.map(parseHandType2);
const combined2: RawLineWithHandtype[] = R.map(
  R.flatten,
  R.zip(rawLines, handTypes2),
);
const combined2withResolvedJokers = combined2.map(resolveJokers);

const sorted2: RawLineWithHandtype[] = R.sort(
  comparer(cardOrder2),
  combined2withResolvedJokers,
);
const values2 = sorted2.map((h, i) => h[1] * (i + 1));

const winnings2 = R.sum(values2);
console.log("part2", winnings2);
