//go:build d24
// +build d24

package Day24

import (
	AH "AoC2024/adventhelper"
	"sort"
	"strings"
)

type Rule struct {
	lhs, rhs, out, mode string
	applied             bool
}

type Node struct {
	state, active bool
}

type Pair struct {
	s string
	b bool
}

func parseNodes(ss []string) map[string]Node {
	ns := make(map[string]Node)
	for _, s := range ss {
		ps := strings.Split(s, ": ")
		ns[ps[0]] = Node{ps[1] != "0", true}
	}

	return ns
}

func parseRules(ss []string, ns map[string]Node) []Rule {
	rs := []Rule{}
	for _, s := range ss {
		ps := strings.Split(s, " ")
		r := Rule{ps[0], ps[2], ps[4], ps[1], false}

		rs = append(rs, r)
		if _, ok := ns[ps[0]]; !ok {
			ns[ps[0]] = Node{false, false}
		}
		if _, ok := ns[ps[2]]; !ok {
			ns[ps[2]] = Node{false, false}
		}
		if _, ok := ns[ps[4]]; !ok {
			ns[ps[4]] = Node{false, false}
		}
	}

	return rs
}

func applyRules(ns map[string]Node, rs []Rule) int {
	all_z_done := false

	for !all_z_done {
		for _, r := range rs {
			if r.applied {
				continue
			}

			if !(ns[r.lhs].active && ns[r.rhs].active) {
				continue
			}

			n := ns[r.out]
			n.active = true
			if r.mode == "AND" {
				n.state = ns[r.lhs].state && ns[r.rhs].state
			} else if r.mode == "OR" {
				n.state = ns[r.lhs].state || ns[r.rhs].state
			} else if r.mode == "XOR" {
				n.state = ns[r.lhs].state != ns[r.rhs].state
			} else {
				panic("Unrecognized mode")
			}
			ns[r.out] = n
			r.applied = true
		}

		all_z_done = true
		for k, v := range ns {
			if AH.FirstRune(k) == 'z' {
				all_z_done = all_z_done && v.active
			}
			if !all_z_done {
				break
			}
		}
	}

	score := 0
	ps := []Pair{}
	for k, v := range ns {
		if AH.FirstRune(k) == 'z' {
			ps = append(ps, Pair{k, v.state})
		}
	}

	sort.Slice(ps, func(i, j int) bool {
		return ps[i].s > ps[j].s
	})
	for _, p := range ps {
		score *= 2
		if p.b {
			score += 1
		}
	}

	return score
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input24.txt")
	ns := []string{}
	rs := []string{}

	for i, s := range ss {
		if i < 90 {
			ns = append(ns, s)
		} else {
			rs = append(rs, s)
		}
	}

	nodes := parseNodes(ns)
	rules := parseRules(rs, nodes)
	p1 := applyRules(nodes, rules)

	// calculated by hand in a spreadsheet. Not sure if there is a simple
	// way to do this programmatically.
	p2 := "bfq,bng,fjp,hkh,hmt,z18,z27,z31"

	AH.PrintSoln(24, p1, p2)
}
