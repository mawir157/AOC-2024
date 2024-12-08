//go:build d08
// +build d08

package Day08

import (
	AH "AoC2024/adventhelper"
)

type Pos struct {
	r, c int
}

func (p Pos) inBounds(r_lim, c_lim int) bool {
	return (p.r >= 0) && (p.r < r_lim) && (p.c >= 0) && (p.c < c_lim)
}

func findAntennae(g []string) map[rune]([]Pos) {
	as := make(map[rune]([]Pos))
	for r, row := range g {
		for c, rn := range row {
			if rn == '.' {
				continue
			}
			as[rn] = append(as[rn], Pos{r, c})
		}
	}

	return as
}

func nodePair(p1, p2 Pos) (Pos, Pos) {
	dr, dc := p1.r-p2.r, p1.c-p2.c

	return Pos{p1.r + dr, p1.c + dc}, Pos{p2.r - dr, p2.c - dc}
}

func nodeLines(p1, p2 Pos, r_lim, c_lim int) map[Pos]bool {
	dr, dc := p1.r-p2.r, p1.c-p2.c
	ns := make(map[Pos]bool)

	p := p1
	for p.inBounds(r_lim, c_lim) {
		ns[p] = true
		p.r -= dr
		p.c -= dc
	}

	p = p2
	for p.inBounds(r_lim, c_lim) {
		ns[p] = true
		p.r += dr
		p.c += dc
	}

	return ns
}

func findAntiNodes(g []string, as map[rune]([]Pos)) (int, int) {
	nodes1 := make(map[Pos]bool)
	nodes2 := make(map[Pos]bool)
	r_lim := len(g)
	c_lim := len(g[0])

	for _, ps := range as {
		for _, p1 := range ps {
			for _, p2 := range ps {
				if p1 == p2 {
					continue
				}

				n1, n2 := nodePair(p1, p2)

				if n1.inBounds(r_lim, c_lim) {
					nodes1[n1] = true
				}
				if n2.inBounds(r_lim, c_lim) {
					nodes1[n2] = true
				}

				nodes := nodeLines(p1, p2, r_lim, c_lim)
				for k, v := range nodes {
					nodes2[k] = v
				}

			}
		}
	}

	return len(nodes1), len(nodes2)
}

func Run() {
	grid, _ := AH.ReadStrFile("../input/input08.txt")
	as := findAntennae(grid)
	p1, p2 := findAntiNodes(grid, as)

	AH.PrintSoln(8, p1, p2)

	return
}
