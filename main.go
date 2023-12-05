package main

import "aoc2023/day5"
import "strings"
import "fmt"
import _ "embed"

//go:embed day5/input
var data string

func main() {
  lines := strings.Split(data, "\n")

  res := day5.Part2(lines)
  fmt.Println("result: ", res)
}
