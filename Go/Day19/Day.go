//go:build d19
// +build d19

package Day19

import (
	AH "AoC2024/adventhelper"
	"strings"
)

type Memo map[string]int

func match(ts []string, s string, m Memo) (count int) {
	if val, ok := m[s]; ok {
		return val
	}

	for _, t := range ts {
		if strings.HasPrefix(s, t) {
			new_s := s[len(t):]
			if (len(new_s)) == 0 {
				count++
			} else {
				count += match(ts, new_s, m)
			}
		}
	}

	m[s] = count
	return
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input19.txt")
	ts := strings.Split(ss[0], ", ")
	ps := []string{}
	for _, s := range ss[2:] {
		ps = append(ps, s)
	}

	p1, p2 := 0, 0

	for _, p := range ps {
		m := make(Memo)
		valid := match(ts, p, m)
		if valid > 0 {
			p1++
		}
		p2 += valid
	}

	AH.PrintSoln(19, p1, p2)
}
