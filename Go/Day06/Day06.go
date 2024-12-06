//go:build d06
// +build d06

package Day06

import (
	AH "AoC2024/adventhelper"
)

type Grid []string
type Dir int

const (
	N Dir = iota
	E
	S
	W
)

func rotate(d Dir) Dir {
	if d == N {
		return E
	} else if d == E {
		return S
	} else if d == S {
		return W
	} else {
		return N
	}
}

type Pos struct {
	r, c int
	d    Dir
}

type Guard struct {
	loc     Pos
	visited map[Pos]int
}

func findGuard(grid Grid) Guard {
	for r, row := range grid {
		for c, char := range row {
			if char == '^' {
				return Guard{Pos{r, c, N}, make(map[Pos]int)}
			}
		}
	}
	return Guard{}
}

func path(g Guard, grid Grid) (Guard, bool) {
	r_max := len(grid)
	c_max := len(grid[0])

	for {
		if _, ok := g.visited[g.loc]; ok {
			return g, true
		}

		g.visited[g.loc]++

		new := g.loc
		if g.loc.d == N {
			new.r--
		} else if g.loc.d == E {
			new.c++
		} else if g.loc.d == S {
			new.r++
		} else {
			new.c--
		}

		if new.r >= r_max || new.r < 0 || new.c >= c_max || new.c < 0 {
			return g, false
		}

		if (grid[new.r][new.c] == '^') || (grid[new.r][new.c] == '.') {
			g.loc = new
		} else { // hit a wall
			g.loc.d = rotate(g.loc.d)
		}
	}

	return g, false
}

func findUniqes(m map[Pos]int) int {
	unique := make(map[Pos]int)
	for p, _ := range m {
		pp := p
		pp.d = N
		unique[pp]++
	}

	return len(unique)
}

func part2(guard Guard, grid Grid) (count int) {
	g_pos := guard.loc
	for r, row := range grid {
		for c, char := range row {
			guard.loc = g_pos
			clear(guard.visited)
			if char == '.' {
				grid[r] = AH.SetRuneAt(row, '#', c)
				_, good := path(guard, grid)
				if good {
					count++
				}
				grid[r] = AH.SetRuneAt(row, '.', c)
			}
		}
	}

	return
}

func Run() {
	grid, _ := AH.ReadStrFile("../input/input06.txt")
	guard := findGuard(grid)
	finished, _ := path(guard, grid)
	p1 := findUniqes(finished.visited)

	p2 := part2(guard, grid)

	AH.PrintSoln(6, p1, p2)

	return
}
