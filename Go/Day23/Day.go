//go:build d23
// +build d23

package Day23

import (
	AH "AoC2024/adventhelper"
	"slices"
	"strings"
)

type Adj [][]int
type Labels map[string]int
type Set map[int]bool

var cache = make(map[int]Set)

func buildGraph(ss []string) (Adj, Labels) {
	label := 0
	map_labels := make(Labels)

	for _, s := range ss {
		ps := strings.Split(s, "-")

		if _, ok := map_labels[ps[0]]; !ok {
			map_labels[ps[0]] = label
			label++
		}
		if _, ok := map_labels[ps[1]]; !ok {
			map_labels[ps[1]] = label
			label++
		}
	}
	adj := make([][]int, len(map_labels))
	for i := range adj {
		adj[i] = make([]int, len(map_labels))
	}

	for _, s := range ss {
		ps := strings.Split(s, "-")
		r := map_labels[ps[0]]
		c := map_labels[ps[1]]
		adj[r][c] = 1
		adj[c][r] = 1
	}

	return adj, map_labels
}

func find(ls Labels, i int) string {
	for k, v := range ls {
		if v == i {
			return k
		}
	}

	return "ERROR"
}

func countTriangles(a Adj, ls Labels) int {
	subgraphs := 0

	for i0 := 0; i0 < len(ls); i0++ {
		for i1 := i0 + 1; i1 < len(ls); i1++ {
			if a[i0][i1] == 0 {
				continue
			}
			for i2 := i1 + 1; i2 < len(ls); i2++ {
				if a[i1][i2] == 0 {
					continue
				}
				if a[i2][i0] == 0 {
					continue
				}

				if AH.FirstRune(find(ls, i0)) == 't' {
					subgraphs++
					continue
				}
				if AH.FirstRune(find(ls, i1)) == 't' {
					subgraphs++
					continue
				}
				if AH.FirstRune(find(ls, i2)) == 't' {
					subgraphs++
				}
			}
		}
	}

	return subgraphs
}

func nbrs(i int, a Adj) Set {
	if val, ok := cache[i]; ok {
		return val
	}

	nbrs := make(Set)
	for idx, v := range a[i] {
		if v == 1 {
			nbrs[idx] = true
		}
	}
	cache[i] = nbrs

	return nbrs
}

func bronKerbosh(R Set, P Set, X Set, a Adj, best *Set) {
	if len(P) == 0 && len(X) == 0 {
		if len(R) > len(*best) {
			*best = R
		}

		return
	}

	PCopy := []int{}
	for v := range P {
		PCopy = append(PCopy, v)
	}

	for _, v := range PCopy {
		RR := make(Set)
		for w := range R {
			RR[w] = true
		}

		RR[v] = true
		NV := nbrs(v, a)
		PP := make(Set)
		XX := make(Set)

		for in := range NV {
			if _, ok := P[in]; ok {
				PP[in] = true
			}
			if _, ok := X[in]; ok {
				XX[in] = true
			}
		}
		bronKerbosh(RR, PP, XX, a, best)

		delete(P, v)
		delete(X, v)
	}

	return
}

func part2(a Adj, ls Labels) string {
	R := make(Set)
	P := make(Set)
	Q := make(Set)
	best := make(Set)

	for i := 0; i < len(ls); i++ {
		P[i] = true
	}

	bronKerbosh(R, P, Q, a, &best)

	vs := []string{}
	for r := range best {
		vs = append(vs, find(ls, r))
	}
	slices.Sort(vs)

	p2 := ""
	for i, s := range vs {
		p2 += s
		if i != len(vs)-1 {
			p2 += ","
		}
	}

	return p2
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input23.txt")
	a, ls := buildGraph(ss)

	p1 := countTriangles(a, ls)
	p2 := part2(a, ls)

	AH.PrintSoln(23, p1, p2)
	return
}
