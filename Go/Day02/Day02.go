//go:build d02
// +build d02

package Day02

import (
	"strconv"
	"strings"

	AH "AoC2024/adventhelper"
)

func parseInput(is []string) [][]int {
	seqs := [][]int{}
	for _, i := range is {
		ps := strings.Split(i, " ")
		seq := []int{}
		for _, p := range ps {
			t, _ := strconv.Atoi(p)
			seq = append(seq, t)
		}
		seqs = append(seqs, seq)
	}

	return seqs
}

func analyse(seq []int) bool {
	diff := AH.SliceDiff(seq)
	first := AH.Sign(diff[0])
	for _, v := range diff {
		if first != AH.Sign(v) {
			return false
		}
		if AH.AbsInt(v) < 1 || AH.AbsInt(v) > 3 {
			return false
		}
	}

	return true
}

func analyse2(seq []int) bool {
	for n := 0; n < len(seq); n++ {
		seqNew := AH.SliceDrop(seq, n)
		if analyse(seqNew) {
			return true
		}
	}

	return false
}

func Run() {
	inputLines, _ := AH.ReadStrFile("../input/input02.txt")
	seqs := parseInput(inputLines)

	part1, part2 := 0, 0
	for _, s := range seqs {
		if analyse(s) {
			part1++
			part2++
		} else {
			if analyse2(s) {
				part2++
			}
		}
	}

	AH.PrintSoln(2, part1, part2)

	return
}
