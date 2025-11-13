//go:build d01
// +build d01

package Day01

import (
	"sort"
	"strconv"
	"strings"

	AH "AoC2024/adventhelper"
)

func parseInput(is []string) ([]int, []int) {
	lhs := []int{}
	rhs := []int{}
	for _, i := range is {
		ps := strings.Split(i, "   ")
		l, _ := strconv.Atoi(ps[0])
		r, _ := strconv.Atoi(ps[1])

		lhs = append(lhs, l)
		rhs = append(rhs, r)
	}

	return lhs, rhs
}

func freq(lhs []int, rhs []int) (s int) {
	f := make(map[int]int)
	for _, v := range lhs {
		_, ok := f[v]
		if !ok {
			for _, u := range rhs {
				if u == v {
					f[v]++
				}
			}
		}
		s += v * f[v]
	}

	return
}

func Run() {
	inputLines, _ := AH.ReadStrFile("../input/input01.txt")
	lhs, rhs := parseInput(inputLines)
	sort.Ints(lhs)
	sort.Ints(rhs)

	part1, part2 := 0, freq(lhs, rhs)
	for i, l := range lhs {
		r := rhs[i]
		part1 += AH.AbsInt(l - r)
	}

	AH.PrintSoln(1, part1, part2)

	return
}
