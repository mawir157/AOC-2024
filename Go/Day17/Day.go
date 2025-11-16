//go:build d17
// +build d17

package Day17

import (
	AH "AoC2024/adventhelper"
	"strconv"
	"strings"
)

type Machine struct {
	A, B, C int
}

type Program []int

func parseInput(ss []string) (Machine, Program) {
	A, _ := strconv.Atoi(ss[0][12:])
	B, _ := strconv.Atoi(ss[1][12:])
	C, _ := strconv.Atoi(ss[2][12:])
	m := Machine{A, B, C}

	vs := ss[3][9:]
	ps := strings.Split(vs, ",")
	prg := Program{}
	for _, p := range ps {
		v, _ := strconv.Atoi(p)
		prg = append(prg, v)
	}

	return m, prg
}

func (m Machine) combo(i int) int {
	if 0 <= i && i <= 3 {
		return i
	}
	if i == 4 {
		return m.A
	}
	if i == 5 {
		return m.B
	}
	if i == 6 {
		return m.C
	}
	return 0
}

func (m Machine) runProg(p Program) Program {
	ptr := 0
	output := Program{}
	for ptr < len(p) {
		ins, opr := p[ptr], p[ptr+1]

		switch ins {
		case 0:
			m.A >>= m.combo(opr)
		case 1:
			m.B ^= opr
		case 2:
			m.B = m.combo(opr) % 8
		case 3:
			if m.A != 0 {
				ptr = opr
			} else {
				ptr += 2
			}
		case 4:
			m.B ^= m.C
		case 5:
			output = append(output, m.combo(opr)%8)
		case 6:
			m.B = m.A >> m.combo(opr)
		case 7:
			m.C = m.A >> m.combo(opr)
		default:
			panic("unrecognized escape character")
		}

		if ins != 3 {
			ptr += 2
		}
	}

	return output
}

func vectorToint(vs []int) int {
	n := 0
	for _, v := range vs {
		n *= 8
		n += v
	}

	return n
}

func backTrace(v *Program, place int, failed bool, prg Program) bool {
	lv := len(*v)
	if (place >= lv) || (place < 0) {
		return true
	}

	start := (*v)[place]
	if failed {
		start++
		for i := place + 1; i < lv; i++ {
			(*v)[i] = 0
		}
	}

	for i := start; i < 8; i++ {
		(*v)[place] = i
		trial := vectorToint(*v)
		m := Machine{trial, 0, 0}
		test := m.runProg(prg)

		if len(test) != len(prg) {
			continue
		}
		if test[lv-1-place] == prg[lv-1-place] {
			return backTrace(v, place+1, false, prg)
		}
	}

	return backTrace(v, place-1, true, prg)
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input17.txt")
	m, prg := parseInput(ss)

	v1 := m.runProg(prg)
	p1 := ""
	for i, v := range v1 {
		p1 += strconv.Itoa(v)
		if i != len(v1)-1 {
			p1 += ","
		}
	}
	v := make(Program, len(prg))
	backTrace(&v, 0, false, prg)
	p2 := vectorToint(v)

	AH.PrintSoln(17, p1, p2)
}
