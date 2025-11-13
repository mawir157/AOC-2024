//go:build d13
// +build d13

package Day13

import (
	AH "AoC2024/adventhelper"
	"strconv"
	"strings"
)

type Game struct {
	ax, ay, bx, by, px, py int
}

func (g Game) minSolve(part2 bool) int {
	if part2 {
		g.px += 10000000000000
		g.py += 10000000000000
	}

	det := g.by*g.ax - g.bx*g.ay
	if det == 0 {
		return 0
	}
	b := g.py*g.ax - g.px*g.ay
	a := g.by*g.px - g.bx*g.py

	score := 0
	if a%det == 0 {
		score += 3 * (a / det)
	} else {
		return 0
	}

	if b%det == 0 {
		score += 1 * (b / det)
	} else {
		return 0
	}

	return score
}

func parseInput(ss []string, c string) []Game {
	gs := []Game{}

	for _, s := range ss {
		g := Game{}
		ps := strings.Split(s, c)

		from, to := 0, 0
		p := ps[0]
		number := ""

		from = strings.Index(p, "+")
		to = strings.Index(p, ",")
		number, p = p[from+1:to], p[to+1:]
		g.ax, _ = strconv.Atoi(number)

		from = strings.Index(p, "+")
		number = p[from+1:]
		g.ay, _ = strconv.Atoi(number)

		p = ps[1]
		from = strings.Index(p, "+")
		to = strings.Index(p, ",")
		number, p = p[from+1:to], p[to+1:]
		g.bx, _ = strconv.Atoi(number)

		from = strings.Index(p, "+")
		number = p[from+1:]
		g.by, _ = strconv.Atoi(number)

		p = ps[2]
		from = strings.Index(p, "=")
		to = strings.Index(p, ",")
		number, p = p[from+1:to], p[to+1:]
		g.px, _ = strconv.Atoi(number)

		from = strings.Index(p, "=")
		number = p[from+1:]
		g.py, _ = strconv.Atoi(number)

		gs = append(gs, g)
	}

	return gs
}

func Run() {
	lgs, _ := AH.ParseLineGroups("../input/input13.txt", "#")
	gs := parseInput(lgs, "#")
	p1, p2 := 0, 0
	for _, g := range gs {
		p1 += g.minSolve(false)
		p2 += g.minSolve(true)
	}

	AH.PrintSoln(13, p1, p2)

	return
}
