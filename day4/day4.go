package day4

import "strings"
import "strconv"

func part1(lines []string) int {
	sum := 0
	for _, l := range lines {
		if l == "" {
			continue
		}
		parts := strings.Split(l, " | ")
		winningNumsRawParts := strings.Split(parts[0], ": ")
		winningNums := strings.Split(winningNumsRawParts[1], " ")
		numbers := strings.Split(parts[1], " ")

		score := 0
	outer:
		for _, n := range numbers {
			if n == "" || n == " " {
				continue
			}
			for _, wn := range winningNums {
				if wn == "" || wn == " " {
					continue
				}
				if wn == n {
					if score == 0 {
						score = 1
					} else {
						score = score * 2
					}
					continue outer
				}
			}
		}
		sum += score
	}
	return sum
}

func Map[A, B any](fn func(A) B, data []A) []B {
	res := make([]B, 0, len(data))
	for _, o := range data {
		res = append(res, fn(o))
	}
	return res
}

func Filter[A any](fn func(A) bool, data []A) []A {
	res := make([]A, 0, len(data))
	for _, o := range data {
		if fn(o) {
			res = append(res, o)
		}
	}
	return res
}

type ScratchCard struct {
	id      int
	winners []int
	numbers []int
}

func strToInt(in string) int {
	parsed, _ := strconv.ParseInt(in, 10, 64)
	return int(parsed)
}

func notEmptyString(in string) bool {
  return in != ""
}

func part2(lines []string) int {
	cards := []*ScratchCard{}
	for _, l := range lines {
		if l == "" {
			continue
		}
		parts := strings.Split(l, " | ")
		winningNumsRawParts := strings.Split(parts[0], ": ")
		idRawParts := strings.Split(winningNumsRawParts[0], " ")
		idRaw := idRawParts[len(idRawParts)-1]
		id := strToInt(idRaw)
		winningNumsRaw := strings.Split(winningNumsRawParts[1], " ")
		numbersRaw := strings.Split(parts[1], " ")

		winningNums := Map(strToInt, Filter(notEmptyString, winningNumsRaw))
		numbers := Map(strToInt, Filter(notEmptyString, numbersRaw))

		cards = append(cards, &ScratchCard{
			id:      id,
			numbers: numbers,
			winners: winningNums,
		})
	}

	cardsMap := make(map[int]*ScratchCard)
	for i := range cards {
		cardsMap[cards[i].id] = cards[i]
	}

	for i := 0; i < len(cards); i++ {
		matches := Filter(func(n int) bool {
			for _, wn := range cards[i].winners {
				if wn == n {
					return true
				}
			}
			return false
		}, cards[i].numbers)
    currentId := cards[i].id
    for j := range matches {
      cards = append(cards, cardsMap[currentId + j + 1])
    }
	}

  return len(cards)

}
