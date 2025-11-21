//go:build d21
// +build d21

package Day21

import (
	AH "AoC2024/adventhelper"
	"strconv"
)

type Pos struct {
	r, c int
}

type LUT map[rune]Pos

// lut := make(g_nPad_moves)

func initialise_NPad() LUT {
	moves := make(LUT)
	moves['0'] = Pos{0, 1}
	moves['A'] = Pos{0, 2}
	moves['1'] = Pos{1, 0}
	moves['2'] = Pos{1, 1}
	moves['3'] = Pos{1, 2}
	moves['4'] = Pos{2, 0}
	moves['5'] = Pos{2, 1}
	moves['6'] = Pos{2, 2}
	moves['7'] = Pos{3, 0}
	moves['8'] = Pos{3, 1}
	moves['9'] = Pos{3, 2}

	moves['^'] = Pos{0, 1}
	moves['<'] = Pos{-1, 0}
	moves['v'] = Pos{-1, 1}
	moves['>'] = Pos{-1, 2}

	return moves
}

func nPadTonPad(nPad string, moves LUT) string {
	cur := 'A'
	instruction := ""
	p_cur := moves[cur]

	for _, next := range nPad {
		p_next := moves[next]
		diff := Pos{p_next.r - p_cur.r, p_next.c - p_cur.c}
		row_first := Pos{p_cur.r + diff.r, p_cur.c} != Pos{0, 0}
		col_first := Pos{p_cur.r, p_cur.c + diff.c} != Pos{0, 0}

		if !row_first { // we must do cols first
			for c := 0; c < AH.AbsInt(diff.c); c++ {
				if diff.c > 0 {
					instruction += ">"
				} else {
					instruction += "<"
				}
			}
			for r := 0; r < AH.AbsInt(diff.r); r++ {
				if diff.r > 0 {
					instruction += "^"
				} else {
					instruction += "v"
				}
			}
		} else if !col_first { // we must do rows first
			for r := 0; r < AH.AbsInt(diff.r); r++ {
				if diff.r > 0 {
					instruction += "^"
				} else {
					instruction += "v"
				}
			}
			for c := 0; c < AH.AbsInt(diff.c); c++ {
				if diff.c > 0 {
					instruction += ">"
				} else {
					instruction += "<"
				}
			}
		} else {
			if diff.r > 0 { // do rows last so we end up near the A
				for c := 0; c < AH.AbsInt(diff.c); c++ {
					if diff.c > 0 {
						instruction += ">"
					} else {
						instruction += "<"
					}
				}
				for r := 0; r < AH.AbsInt(diff.r); r++ {
					if diff.r > 0 {
						instruction += "^"
					} else {
						instruction += "v"
					}
				}
			} else {
				for r := 0; r < AH.AbsInt(diff.r); r++ {
					if diff.r > 0 {
						instruction += "^"
					} else {
						instruction += "v"
					}
				}
				for c := 0; c < AH.AbsInt(diff.c); c++ {
					if diff.c > 0 {
						instruction += ">"
					} else {
						instruction += "<"
					}
				}
			}
		}

		instruction += "A"
		cur = next
		p_cur = moves[cur]
	}

	return instruction
}

func expandNumberPad(pairs map[string]int) map[string]int {
	new_pairs := make(map[string]int)
	for k, v := range pairs {
		// A*
		if k == "AA" {
			new_pairs["AA"] += v
		}
		if k == "A^" {
			new_pairs["A<"] += v
			new_pairs["<A"] += v
		}
		if k == "A<" {
			new_pairs["Av"] += v
			new_pairs["v<"] += v
			new_pairs["<<"] += v
			new_pairs["<A"] += v
		}
		if k == "Av" {
			new_pairs["A<"] += v
			new_pairs["<v"] += v
			new_pairs["vA"] += v
		}
		if k == "A>" {
			new_pairs["Av"] += v
			new_pairs["vA"] += v
		}
		// ^*
		if k == "^A" {
			new_pairs["A>"] += v
			new_pairs[">A"] += v
		}
		if k == "^^" {
			new_pairs["AA"] += v
		}
		if k == "^<" {
			new_pairs["Av"] += v
			new_pairs["v<"] += v
			new_pairs["<A"] += v
		}
		if k == "^v" {
			new_pairs["Av"] += v
			new_pairs["vA"] += v
		}
		if k == "^>" {
			new_pairs["Av"] += v
			new_pairs["v>"] += v
			new_pairs[">A"] += v
		}
		// <*
		if k == "<A" {
			new_pairs["A>"] += v
			new_pairs[">>"] += v
			new_pairs[">^"] += v
			new_pairs["^A"] += v
		}
		if k == "<^" {
			new_pairs["A>"] += v
			new_pairs[">^"] += v
			new_pairs["^A"] += v
		}
		if k == "<<" {
			new_pairs["AA"] += v
		}
		if k == "<v" {
			new_pairs["A>"] += v
			new_pairs[">A"] += v
		}
		if k == "<>" {
			new_pairs["A>"] += v
			new_pairs[">>"] += v
			new_pairs[">A"] += v
		}
		// v*
		if k == "vA" {
			new_pairs["A^"] += v
			new_pairs["^>"] += v
			new_pairs[">A"] += v
		}
		if k == "v^" {
			new_pairs["A^"] += v
			new_pairs["^A"] += v
		}
		if k == "v<" {
			new_pairs["A<"] += v
			new_pairs["<A"] += v
		}
		if k == "vv" {
			new_pairs["AA"] += v
		}
		if k == "v>" {
			new_pairs["A>"] += v
			new_pairs[">A"] += v
		}
		// >*
		if k == ">A" {
			new_pairs["A^"] += v
			new_pairs["^A"] += v
		}
		if k == ">^" {
			new_pairs["A<"] += v
			new_pairs["<^"] += v
			new_pairs["^A"] += v
		}
		if k == "><" {
			new_pairs["A<"] += v
			new_pairs["<<"] += v
			new_pairs["<A"] += v
		}
		if k == ">v" {
			new_pairs["A<"] += v
			new_pairs["<A"] += v
		}
		if k == ">>" {
			new_pairs["AA"] += v
		}
	}

	return new_pairs
}

func score(s string, moves LUT, robots int) int {
	r, _ := strconv.Atoi(s[:len(s)-1])
	robot_input := nPadTonPad(s, moves)

	pairs := make(map[string]int)
	cur := 'A'
	for _, next := range robot_input {
		key := string(cur)
		key += string(next)
		pairs[key] += 1

		cur = next
	}

	for i := 0; i < robots; i++ {
		new_pairs := expandNumberPad(pairs)
		pairs = new_pairs
	}

	score := 0
	for _, v := range pairs {
		score += v
	}

	return r * score
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input21.txt")
	nPad := initialise_NPad()

	p1, p2 := 0, 0
	for _, s := range ss {
		p1 += score(s, nPad, 2)

		p2 += score(s, nPad, 25)
	}

	AH.PrintSoln(21, p1, p2)
}
