//go:build d03
// +build d03

package Day03

import (
	"regexp"
	"strconv"
	"strings"

	AH "AoC2024/adventhelper"
)

func findMul(s string) (p1 int, p2 int) {
	p1, p2 = 0, 0
	on := true

	r, _ := regexp.Compile("mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)")
	ps := r.FindAllString(s, -1)

	for _, p := range ps {
		if p == "do()" {
			on = true
		} else if p == "don't()" {
			on = false
		} else {
			p = AH.Drop(AH.Init(p), 4)
			ps := strings.Split(p, ",")

			l, _ := strconv.Atoi(ps[0])
			r, _ := strconv.Atoi(ps[1])

			p1 += (l * r)
			if on {
				p2 += (l * r)
			}
		}
	}

	return
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input03.txt")
	s := strings.Join(ss, "")

	part1, part2 := findMul(s)

	AH.PrintSoln(3, part1, part2)

	return
}
