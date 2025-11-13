//go:build d11
// +build d11

package Day11

import (
	AH "AoC2024/adventhelper"
	"strconv"
	"strings"
)

func parseInput(s string) map[int]int {
	arr := make(map[int]int)
	ps := strings.Split(s, " ")
	for _, p := range ps {
		n, _ := strconv.Atoi(p)
		arr[n]++
	}

	return arr
}

func digits(x int) (n int) {
	n = 0
	for x > 0 {
		n++
		x /= 10
	}

	return
}

func blink(m map[int]int) map[int]int {
	arr := make(map[int]int)
	for v, c := range m {
		if v == 0 {
			arr[1] += c
			continue
		}

		n := digits(v)
		if n%2 == 0 {
			tens := AH.PowInt(10, n/2)
			l := v / tens
			r := v % tens
			arr[l] += c
			arr[r] += c
			continue
		}

		arr[2024*v] += c
	}

	return arr
}

func blinkAgain(n int, m map[int]int) (count int) {
	for i := 0; i < n; i++ {
		m = blink(m)
	}
	count = 0
	for _, c := range m {
		count += c
	}

	return
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input11.txt")
	m := parseInput(ss[0])
	p1, p2 := blinkAgain(25, m), blinkAgain(75, m)

	AH.PrintSoln(11, p1, p2)

	return
}
