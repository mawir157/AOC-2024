//go:build d20
// +build d20

package Day20

import (
	AH "AoC2024/adventhelper"
)

type Pos struct {
	r, c int
}

type CELL int

const (
	SPACE CELL = iota
	WALL
)

type Grid [][]CELL

func parseInput(ss []string) (Grid, Pos, Pos) {
	g := Grid{}
	start := Pos{0, 0}
	end := Pos{0, 0}

	for ri, s := range ss {
		row := []CELL{}
		for ci, rn := range s {
			if rn == '#' {
				row = append(row, WALL)
			} else {
				row = append(row, SPACE)
				if rn == 'S' {
					start = Pos{ri, ci}
				} else if rn == 'E' {
					end = Pos{ri, ci}
				}
			}
		}
		g = append(g, row)
	}

	return g, start, end
}

func nbrs(p Pos, Q AH.Set[Pos]) []Pos {
	ns := []Pos{}

	pN := Pos{p.r - 1, p.c}
	if _, ok := Q[pN]; ok {
		ns = append(ns, pN)
	}
	pE := Pos{p.r, p.c + 1}
	if _, ok := Q[pE]; ok {
		ns = append(ns, pE)
	}
	pS := Pos{p.r + 1, p.c}
	if _, ok := Q[pS]; ok {
		ns = append(ns, pS)
	}
	pW := Pos{p.r, p.c - 1}
	if _, ok := Q[pW]; ok {
		ns = append(ns, pW)
	}

	return ns
}

func findMin(Q AH.Set[Pos], M map[Pos]int) (Pos, bool) {
	min := 1000000
	min_p := Pos{-1, -1}
	found := false
	for p := range Q {
		if d, ok := M[p]; ok && d < min {
			min = d
			min_p = p
			found = true
		}
	}

	return min_p, found
}

func dijkstra(g Grid, start Pos) map[Pos]int {
	dist := make(map[Pos]int)
	Q := make(AH.Set[Pos])
	flagged := make(AH.Set[Pos])

	for ri, row := range g {
		for ci, c := range row {
			if c == SPACE {
				p := Pos{ri, ci}
				dist[p] = 1000000
				Q[p] = true
			}
		}
	}

	dist[start] = 0
	flagged[start] = true

	for len(Q) > 0 {
		minU, found := findMin(flagged, dist)
		if !found {
			break
		}
		delete(Q, minU)
		delete(flagged, minU)

		ns := nbrs(minU, Q)

		for _, n := range ns {
			incr := 1
			alt := dist[minU] + incr
			if alt < dist[n] {
				dist[n] = alt
				flagged[n] = true
			}
		}
	}

	return dist
}

func findShortCuts(dist map[Pos]int, full int, steps int) map[int]int {
	ss := make(map[int]int)

	for p, d := range dist {
		for dr := -steps; dr <= steps; dr++ {
			for dc := -steps; dc <= steps; dc++ {
				dd := AH.AbsInt(dr) + AH.AbsInt(dc)
				if dd > steps {
					continue
				}

				shortcut := Pos{p.r + dr, p.c + dc}
				if _, ok := dist[shortcut]; ok {
					new_route := d + dd + full - dist[shortcut]
					if new_route < full {
						ss[full-new_route] += 1
					}
				}
			}
		}
	}

	return ss
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input20.txt")
	grid, start, end := parseInput(ss)
	dist := dijkstra(grid, start)
	ss1 := findShortCuts(dist, dist[end], 2)
	p1 := 0
	for p, c := range ss1 {
		if p >= 100 {
			p1 += c
		}
	}

	ss2 := findShortCuts(dist, dist[end], 20)
	p2 := 0
	for p, c := range ss2 {
		if p >= 100 {
			p2 += c
		}
	}

	AH.PrintSoln(20, p1, p2)
}
