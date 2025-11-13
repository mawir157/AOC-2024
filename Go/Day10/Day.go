//go:build d10
// +build d10

package Day10

import (
	AH "AoC2024/adventhelper"
)

type Grid [][]int
type Pos struct {
	r, c int
}

func parseInput(ss []string) (Grid, []Pos) {
	g := Grid{}
	starts := []Pos{}

	for r, row := range ss {
		rs := []int{}
		for c, rn := range row {
			rs = append(rs, int(rn-'0'))
			if rn == '0' {
				starts = append(starts, Pos{r, c})
			}
		}
		g = append(g, rs)
	}

	return g, starts
}

func nbrs(p Pos, g Grid) []Pos {
	cur := g[p.r][p.c]
	m_r := len(g)
	m_c := m_r

	ns := []Pos{}
	if (p.r-1 >= 0) && (g[p.r-1][p.c] == cur+1) {
		ns = append(ns, Pos{p.r - 1, p.c})
	}
	if (p.c-1 >= 0) && (g[p.r][p.c-1] == cur+1) {
		ns = append(ns, Pos{p.r, p.c - 1})
	}
	if (p.r+1 < m_r) && (g[p.r+1][p.c] == cur+1) {
		ns = append(ns, Pos{p.r + 1, p.c})
	}
	if (p.c+1 < m_c) && (g[p.r][p.c+1] == cur+1) {
		ns = append(ns, Pos{p.r, p.c + 1})
	}

	return ns
}

func ends(g Grid, p Pos, es map[Pos]int) {
	cur := g[p.r][p.c]
	if cur == 9 {
		es[p]++
		return
	}

	ns := nbrs(p, g)
	for _, n := range ns {
		ends(g, n, es)
	}

	return
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input10.txt")
	g, starts := parseInput(ss)
	part1, part2 := 0, 0
	es := make(map[Pos]int)
	for _, s := range starts {
		clear(es)
		ends(g, s, es)
		part1 += len(es)
		for _, v := range es {
			part2 += v
		}
	}

	AH.PrintSoln(10, part1, part2)

	return
}
