//go:build d16
// +build d16

package Day16

import (
	AH "AoC2024/adventhelper"
)

type Pos struct {
	r, c, d int
}

type CELL int

const (
	SPACE CELL = iota
	WALL
)

type Grid [][]CELL

type Pair struct {
	p Pos
	i int
}

func parseMaze(ss []string) (Grid, Pos, Pos) {
	g := Grid{}
	start := Pos{0, 0, 0}
	end := Pos{0, 0, 0}
	for r, s := range ss {
		row := []CELL{}
		for c, ch := range s {
			if ch == '#' {
				row = append(row, WALL)
			} else {
				row = append(row, SPACE)
				if ch == 'S' {
					start.r = r
					start.c = c
					start.d = 1
				}
				if ch == 'E' {
					end.r = r
					end.c = c
					end.d = 0
				}
			}
		}
		g = append(g, row)
	}

	return g, start, end
}

func nbrs(g Grid, p Pos, Q [][][]bool) []Pos {
	ns := []Pos{}
	ppos := Pos{p.r, p.c, (p.d + 3) % 4}
	if Q[ppos.r][ppos.c][ppos.d] {
		ns = append(ns, ppos)
	}
	pneg := Pos{p.r, p.c, (p.d + 1) % 4}
	if Q[pneg.r][pneg.c][pneg.d] {
		ns = append(ns, pneg)
	}

	pnew := p
	switch p.d {
	case 0:
		pnew.r -= 1
	case 1:
		pnew.c += 1
	case 2:
		pnew.r += 1
	case 3:
		pnew.c -= 1
	}

	if g[pnew.r][pnew.c] == SPACE && Q[pnew.r][pnew.c][pnew.d] {
		ns = append(ns, pnew)
	}

	return ns
}

func countBacktrack(p Pos, prev map[Pos]([]Pos), routes *[][]bool) {
	(*routes)[p.r][p.c] = true

	if _, ok := prev[p]; !ok {
		return
	} else {
		ps := prev[p]
		for _, pp := range ps {
			(*routes)[p.r][p.c] = true
			countBacktrack(pp, prev, routes)
		}
	}

	return
}

func dijkstra(g Grid, start Pos, end Pos) (int, int) {
	dist := make([][][]int, len(g))
	prev := make(map[Pos]([]Pos))
	Q := make([][][]bool, len(g))
	flagged := []Pair{}

	for ri, rr := range g {
		dist[ri] = make([][]int, len(rr))
		Q[ri] = make([][]bool, len(rr))
		for ci, _ := range rr {
			dist[ri][ci] = make([]int, 4)
			Q[ri][ci] = make([]bool, 4)
			for di := 0; di < 4; di++ {
				if g[ri][ci] == SPACE {
					dist[ri][ci][di] = 1000000
					Q[ri][ci][di] = true
				}
			}
		}
	}

	dist[start.r][start.c][start.d] = 0
	alt := 0
	flagged = append(flagged, Pair{start, 0})
	pairU := Pair{}

	for len(flagged) > 0 {
		pairU, flagged = flagged[0], flagged[1:]
		minU := pairU.p

		Q[minU.r][minU.c][minU.d] = false
		ns := nbrs(g, minU, Q)

		for _, n := range ns {
			d_step := 1
			if n.d != minU.d {
				d_step = 1000
			}
			alt = dist[minU.r][minU.c][minU.d] + d_step

			if alt < dist[n.r][n.c][n.d] {
				dist[n.r][n.c][n.d] = alt
				prev[n] = []Pos{minU}

				pair := Pair{n, alt}
				at_end := true
				for ii, pr := range flagged {
					if alt < pr.i {
						flagged = append(flagged[:ii+1], flagged[ii:]...)
						flagged[ii] = pair
						at_end = false
					}
				}
				if at_end {
					flagged = append(flagged, pair)
				}
			} else if alt == dist[n.r][n.c][n.d] {
				v := prev[n]
				if minU != v[len(v)-1] {
					prev[n] = append(prev[n], minU)
				}
			}
		}
	}

	routes := make([][]bool, len(g))
	for i := range routes {
		routes[i] = make([]bool, len(g))
	}

	countBacktrack(end, prev, &routes)
	counter := 0
	for _, r := range routes {
		for _, c := range r {
			if c {
				counter += 1
			}
		}
	}

	return dist[end.r][end.c][end.d], counter
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input16.txt")
	grid, start, end := parseMaze(ss)

	p1, p2 := dijkstra(grid, start, end)

	AH.PrintSoln(17, p1, p2)
}
