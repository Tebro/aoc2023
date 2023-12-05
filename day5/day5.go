package day5

import "strings"
import "strconv"
import "sort"

func Filter[A any](fn func(A) bool, data []A) []A {
	res := make([]A, 0, len(data))
	for _, o := range data {
		if fn(o) {
			res = append(res, o)
		}
	}
	return res
}

type rawMap struct {
	name  string
	from  string
	to    string
	rules [][]int
}

func (rm *rawMap) Locate(src int) int {
	for _, r := range rm.rules {
    if src >= r[1] && src < r[1]+r[2] {
      diff := src - r[1]
      return r[0]+diff
    }
	}

	return src
}

func parseInput(lines []string) ([]int, []*rawMap) {
	seedsLineParts := strings.Split(strings.Split(lines[0], ": ")[1], " ")
	seeds := make([]int, len(seedsLineParts))
	for i, s := range seedsLineParts {
		parsed, _ := strconv.ParseInt(s, 10, 64)
		seeds[i] = int(parsed)
	}

	maps := []*rawMap{}

	for i := 2; i < len(lines); i++ {
		line := lines[i]
		if strings.Contains(line, "map") {
			name := strings.Split(line, " ")[0]
			nameParts := strings.Split(name, "-to-")

			m := rawMap{
				name:  name,
				from:  nameParts[0],
				to:    nameParts[1],
				rules: [][]int{},
			}
			maps = append(maps, &m)
			continue
		}
		if line == "" {
			continue // empyt line between maps
		}
		ruleParts := strings.Split(line, " ")
		rule := make([]int, 3)
		for i, rp := range ruleParts {
			parsed, _ := strconv.ParseInt(rp, 10, 64)
			rule[i] = int(parsed)
		}
		maps[len(maps)-1].rules = append(maps[len(maps)-1].rules, rule)
	}

	return seeds, maps
}

func recur(targetType string, currentType string, currentId int, maps []*rawMap) int {
	if currentType == targetType {
		return currentId
	}

	relevantMaps := Filter(func(m *rawMap) bool {
		return m.from == currentType
	}, maps)

	for _, rm := range relevantMaps {

		nextType := rm.to
		nextId := rm.Locate(currentId)
		traversalResult := recur(targetType, nextType, nextId, maps)
		if traversalResult > 0 {
			return traversalResult
		}
	}
	return -1
}

func part1(lines []string) int {
	seeds, maps := parseInput(lines)

	locations := make([]int, 0, len(seeds))
	for _, id := range seeds {
		res := recur("location", "seed", id, maps)
		locations = append(locations, res)
	}

	sort.Ints(locations)

	return locations[0]
}

func Part2(lines []string) int {
	seeds, maps := parseInput(lines)

  res := -1
  for i:= 0; i < len(seeds); i += 2 {
    for j := 0; j < seeds[i+1]; j++ {
		  loc := recur("location", "seed", seeds[i]+j, maps)
      if res == -1 {
        res = loc
      }
      if loc < res {
        res = loc
      }
    }
  }

	return res
}
