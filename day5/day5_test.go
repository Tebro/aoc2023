package day5

import _ "embed"
import "testing"
import "strings"

//go:embed input
var data string


func Test_runpart1(t *testing.T) {
  lines := strings.Split(data, "\n")

  res := part1(lines)
  t.Log(res)
  t.Fail()
}

//func Test_runpart2(t *testing.T) {
//  lines := strings.Split(data, "\n")
//
//  res := part2(lines)
//  t.Log(res)
//  t.Fail()
//}

//func Benchmark_part1(b *testing.B) {
//  lines := strings.Split(data, "\n")
//
//  b.ResetTimer()
//	for i := 0; i < b.N; i++ {
//		part1(lines)
//	}
//}
