//go:build d05
// +build d05

package Day05

import (
	AH "AoC2024/adventhelper"
	"strconv"
	"strings"
)

type Rule struct {
	lhs, rhs int
}

func parseRulesToBook(s string) []Rule {
	rules := []Rule{}
	ss := strings.Split(s, "#")

	for _, r := range ss {
		ps := strings.Split(r, "|")

		l, _ := strconv.Atoi(ps[0])
		r, _ := strconv.Atoi(ps[1])

		rules = append(rules, Rule{l, r})
	}

	return rules
}

func sortBook(book []int, rules []Rule) ([]int, int) {
	counter := 0
	// bubble sort book into order
	for iters := 0; iters < len(book); iters++ {
		for i := 0; i < len(book)-1; i++ {
			for j := i + 1; j < len(book); j++ {
				for _, r := range rules {
					if (r.lhs == book[j]) && (r.rhs == book[i]) {
						temp := book[i]
						book[i] = book[j]
						book[j] = temp
						counter++
					}
				}
			}
		}
	}

	return book, counter
}

func parseBooks(s string) (bs [][]int) {
	ss := strings.Split(s, "#")
	for _, r := range ss {
		b := []int{}
		ps := strings.Split(r, ",")
		for _, p := range ps {
			pg, _ := strconv.Atoi(p)
			b = append(b, pg)
		}
		bs = append(bs, b)
	}

	return
}

func Run() {
	g, _ := AH.ParseLineGroups("../input/input05.txt", "#")
	rules := parseRulesToBook(g[0])
	books := parseBooks(g[1])

	part1, part2 := 0, 0

	for _, book := range books {
		ordered, count := sortBook(book, rules)
		if count == 0 {
			part1 += ordered[len(book)/2]
		} else {
			part2 += ordered[len(book)/2]
		}
	}

	AH.PrintSoln(5, part1, part2)

	return
}
