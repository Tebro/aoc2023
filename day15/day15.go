package main

import _ "embed"
import "strings"
import "fmt"
import "strconv"

//go:embed input
var data string

func hashString(input string) int {
	partSum := 0
	for _, c := range input {
		partSum += int(c)
		partSum = partSum * 17
		partSum = partSum % 256
	}
	return partSum
}

func removeIndex[T any](s []T, index int) []T {
	return append(s[:index], s[index+1:]...)
}

type lens struct {
	label string
	value int
}

func indexOf(ls []lens, l lens) int {
  for i, b := range ls {
    if b.label == l.label {
      return i
    }
  }
  return -1
}

func main() {
	inParts := strings.Split(strings.TrimSpace(data), ",")

	sum := 0
	for _, part := range inParts {
		sum += hashString(part)
	}
	fmt.Println("part1", sum)

	data := make(map[int][]lens)
	for _, part := range inParts {
		isRemoveOp := strings.ContainsRune(part, '-')

		var rawLabel string
		var v int
		if isRemoveOp {
			ps := strings.Split(part, "-")
			rawLabel = ps[0]
		} else {
			ps := strings.Split(part, "=")
			rawLabel = ps[0]
      vRaw, _ := strconv.ParseInt(ps[1], 10, 64)
			v = int(vRaw)
		}

		h := hashString(rawLabel)
		l := lens{label: rawLabel, value: v}
		old, ok := data[h]

		if isRemoveOp {
      if ok {
				existIdx := indexOf(old, l)
        if existIdx >= 0 {
          data[h] = removeIndex(data[h], existIdx)
        }
      }
		} else {
			if !ok {
				data[h] = []lens{l}
			} else {
				existIdx := indexOf(old, l)
				if existIdx >= 0 {
					data[h][existIdx].value = v
				} else {
					data[h] = append(data[h], l)
				}
			}
		}
	}
  sum2 := 0
  for k, v := range data {
    for i, l := range v {
      sum2 += (k+1) * (i+1) * l.value
    }
  }
  //fmt.Printf("%v\n", data)
  fmt.Println("part2", sum2)
}
