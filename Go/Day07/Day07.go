//go:build d07
// +build d07

package Day07

import (
	AH "AoC2024/adventhelper"
	"strconv"
	"strings"
)

func parseInput(s string) (int, []int) {
	ps := strings.Split(s, ": ")
	t, _ := strconv.Atoi(ps[0])
	arr := []int{}
	as := strings.Split(ps[1], " ")
	for _, a := range as {
		b, _ := strconv.Atoi(a)
		arr = append(arr, b)
	}

	return t, arr
}

func concat(x, y int) int {
	n := 0
	yy := y
	for yy > 0 {
		n++
		yy /= 10
	}

	return x*AH.PowInt(10, n) + y
}

func countdown(acc, ptr int, target int, arr []int, mode bool) bool {
	if acc > target {
		return false
	}
	if len(arr) == ptr {
		return (acc == target)
	}
	x := arr[ptr]

	if mode {
		return countdown(acc+x, ptr+1, target, arr, mode) ||
			countdown(acc*x, ptr+1, target, arr, mode) ||
			countdown(concat(acc, x), ptr+1, target, arr, mode)
	} else {
		return countdown(acc+x, ptr+1, target, arr, mode) ||
			countdown(acc*x, ptr+1, target, arr, mode)
	}
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input07.txt")
	p1, p2 := 0, 0
	for _, s := range ss {
		t, xs := parseInput(s)
		if countdown(0, 0, t, xs, false) {
			p1 += t
		}
		if countdown(0, 0, t, xs, true) {
			p2 += t
		}
	}

	AH.PrintSoln(7, p1, p2)

	return
}
