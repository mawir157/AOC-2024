//go:build d12
// +build d12

package Day12

import (
	AH "AoC2024/adventhelper"
	"fmt"
)

type Region struct {
	size, perimeter, corners int
}

type Pos struct {
	r, c int
}

func parseInput(ss []string) map[Pos]rune {
	grid := make(map[Pos]rune)
	for r, s := range ss {
		for c, rn := range s {
			grid[Pos{r, c}] = rn
		}
	}

	return grid
}

func part1(grid map[Pos]rune) []Region {
	regions := []Region{}
	grid_copy := make(map[Pos]rune)
	for k, v := range grid {
		grid_copy[k] = v
	}
	ps := []Pos{}

	for len(grid_copy) > 0 {
		ch := rune(0)
		p := Pos{0, 0}
		for p, ch = range grid_copy {
			break
		}
		delete(grid_copy, p)
		ps = append(ps, p)

		size, perimeter, corners := 0, 0, 0

		for len(ps) > 0 {
			size++
			p, ps = ps[0], ps[1:]
			nf, ef, sf, wf := false, false, false, false

			north := Pos{p.r - 1, p.c}
			if val, ok := grid_copy[north]; ok && (val == ch) {
				delete(grid_copy, north)
				ps = append(ps, north)
			} else {
				if grid[north] != ch {
					perimeter++
					nf = true
				}
			}

			east := Pos{p.r, p.c + 1}
			if val, ok := grid_copy[east]; ok && (val == ch) {
				delete(grid_copy, east)
				ps = append(ps, east)
			} else {
				if grid[east] != ch {
					perimeter++
					ef = true
				}
			}

			south := Pos{p.r + 1, p.c}
			if val, ok := grid_copy[south]; ok && (val == ch) {
				delete(grid_copy, south)
				ps = append(ps, south)
			} else {
				if grid[south] != ch {
					perimeter++
					sf = true
				}
			}

			west := Pos{p.r, p.c - 1}
			if val, ok := grid_copy[west]; ok && (val == ch) {
				delete(grid_copy, west)
				ps = append(ps, west)
			} else {
				if grid[west] != ch {
					perimeter++
					wf = true
				}
			}

			if nf && ef {
				corners++
			}
			if ef && sf {
				corners++
			}
			if sf && wf {
				corners++
			}
			if wf && nf {
				corners++
			}

			if !(nf || ef) {
				NE := Pos{p.r - 1, p.c + 1}
				if grid[NE] != ch {
					corners++
				}
			}

			if !(ef || sf) {
				SE := Pos{p.r + 1, p.c + 1}
				if grid[SE] != ch {
					corners++
				}
			}

			if !(sf || wf) {
				SW := Pos{p.r + 1, p.c - 1}
				if grid[SW] != ch {
					corners++
				}
			}

			if !(wf || nf) {
				NW := Pos{p.r - 1, p.c - 1}
				if grid[NW] != ch {
					corners++
				}
			}
		}
		regions = append(regions, Region{size, perimeter, corners})
	}
	return regions
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input12.txt")
	m := parseInput(ss)
	regions := part1(m)
	fmt.Println(len(regions))
	p1, p2 := 0, 0
	for _, rgn := range regions {
		p1 += rgn.size * rgn.perimeter
		p2 += rgn.size * rgn.corners
	}

	AH.PrintSoln(12, p1, p2)

	return
}
