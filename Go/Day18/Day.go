//go:build d18
// +build d18

package Day18

import (
	AH "AoC2024/adventhelper"
	"strconv"
	"strings"
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

func parseInput(ss []string, kb int) Grid {
	g := make(Grid, 71)
	for ii := range g {
		g[ii] = make([]CELL, 71)
	}
	counter := 0
	for _, s := range ss {
		ps := strings.Split(s, ",")
		r, _ := strconv.Atoi(ps[0])
		c, _ := strconv.Atoi(ps[1])
		g[r][c] = WALL
		counter++
		if counter >= kb {
			break
		}
	}

	return g
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

func dijkstra(g Grid, start Pos, end Pos) (int, bool) {
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

	return dist[end], dist[end] != 1000000
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input18.txt")
	g := parseInput(ss, 1024)
	p1, _ := dijkstra(g, Pos{0, 0}, Pos{70, 70})
	p2 := ""
	for kbs := len(ss) - 1; kbs >= 0; kbs-- {
		g := parseInput(ss, kbs)
		_, done := dijkstra(g, Pos{0, 0}, Pos{70, 70})
		if done {
			p2 = ss[kbs]
			break
		}
	}

	AH.PrintSoln(18, p1, p2)
}
