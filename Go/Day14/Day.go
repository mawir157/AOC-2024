//go:build d14
// +build d14

package Day14

import (
	AH "AoC2024/adventhelper"
	"strconv"
	"strings"
)

type Pos struct {
	r, c int
}

type Point struct {
	x, y, dx, dy int
}

func afterNSteps(n int, p Point, wx int, wy int) Pos {
	return Pos{((p.x+n*p.dx)%wx + wx) % wx, ((p.y+n*p.dy)%wy + wy) % wy}
}

func quadCount(vs [4]int, p Pos, wx int, wy int) [4]int {
	if p.r < wx/2 {
		if p.c < wy/2 {
			vs[0]++
		} else if p.c > wy/2 {
			vs[1]++
		}
	} else if p.r > wx/2 {
		if p.c < wy/2 {
			vs[2]++
		} else if p.c > wy/2 {
			vs[3]++
		}
	}

	return vs
}

func parseInput(s string) Point {
	p := Point{0, 0, 0, 0}
	from, to := 0, 0

	sn := ""
	from = strings.Index(s, "=")
	to = strings.Index(s, ",")
	sn, s = s[from+1:to], s[to:]
	p.x, _ = strconv.Atoi(sn)

	from = 0
	to = strings.Index(s, " ")
	sn, s = s[from+1:to], s[to+1:]
	p.y, _ = strconv.Atoi(sn)

	from = strings.Index(s, "=")
	to = strings.Index(s, ",")
	sn, s = s[from+1:to], s[to:]
	p.dx, _ = strconv.Atoi(sn)

	from = 0
	to = strings.Index(s, " ")
	sn = s[from+1:]
	p.dy, _ = strconv.Atoi(sn)

	return p
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input14.txt")
	ps := []Point{}
	quads := [4]int{0, 0, 0, 0}
	wx, wy := 101, 103
	for _, s := range ss {
		p := parseInput(s)
		ps = append(ps, p)
		newP := afterNSteps(100, p, wx, wy)
		quads = quadCount(quads, newP, wx, wy)
	}
	p1 := quads[0] * quads[1] * quads[2] * quads[3]
	p2 := 0

	for p2 = wx * wy; p2 > 0; p2-- {
		rowCount := make([]int, wx)
		colCount := make([]int, wy)

		for _, p := range ps {
			pn := afterNSteps(p2, p, wx, wy)
			rowCount[pn.r]++
			colCount[pn.c]++
		}

		topr := 0
		for _, rc := range rowCount {
			if rc > topr {
				topr = rc
			}
		}

		topc := 0
		for _, rc := range colCount {
			if rc > topc {
				topc = rc
			}
		}

		if topc > 30 && topr > 30 {
			break
		}
	}

	AH.PrintSoln(14, p1, p2)

	return
}
