//go:build d15
// +build d15

package Day15

import (
	AH "AoC2024/adventhelper"
	"slices"
	"sort"
	"strings"
)

type CELL int

const (
	SPACE CELL = iota
	BOX
	WALL
	BOXL
	BOXR
)

type Pos struct {
	r, c int
}

func add(lhs, rhs Pos) Pos {
	lhs.r += rhs.r
	lhs.c += rhs.c

	return lhs
}

func sub(lhs, rhs Pos) Pos {
	lhs.r -= rhs.r
	lhs.c -= rhs.c

	return lhs
}

type Warehouse [][]CELL
type Robot Pos

func parseWarehouse(s string, part1 bool) (Warehouse, Pos) {
	w := Warehouse{}
	r := Pos{0, 0}
	// int r_i := 0
	input_rows := strings.Split(s, "|")
	for r_i, input_row := range input_rows {
		row := []CELL{}
		for c_i, c := range input_row {
			if c == '.' {
				row = append(row, SPACE)
				if !part1 {
					row = append(row, SPACE)
				}
			} else if c == 'O' {
				if part1 {
					row = append(row, BOX)
				} else {
					row = append(row, BOXL)
					row = append(row, BOXR)
				}
			} else if c == '#' {
				row = append(row, WALL)
				if !part1 {
					row = append(row, WALL)
				}
			} else if c == '@' {
				row = append(row, SPACE)
				r.r = r_i
				r.c = c_i
				if !part1 {
					r.c += c_i
					row = append(row, SPACE)
				}
			}
		}
		w = append(w, row)
	}

	return w, r
}

func moveRobot(r *Pos, w *Warehouse, c rune) bool {
	step := Pos{0, 0}
	if c == '<' {
		step = Pos{0, -1}
	} else if c == '^' {
		step = Pos{-1, 0}
	} else if c == '>' {
		step = Pos{0, 1}
	} else if c == 'v' {
		step = Pos{1, 0}
	} else {
		return false
	}

	r_new := add(*r, step)
	cell := (*w)[r_new.r][r_new.c]

	if cell == WALL {
		return false
	}
	if cell == SPACE {
		*r = r_new
		return true
	}
	if cell == BOX {
		box := r_new
		cell_end := (*w)[box.r][box.c]
		for cell_end == BOX {
			box = add(box, step)
			cell_end = (*w)[box.r][box.c]
		}
		if cell_end == WALL {
			return false
		}
		if cell_end == SPACE {
			(*w)[r_new.r][r_new.c], (*w)[box.r][box.c] = (*w)[box.r][box.c], (*w)[r_new.r][r_new.c]
			*r = r_new
			return true
		}
	}

	if cell == BOXL || cell == BOXR {
		if (c == '<') || (c == '>') { // move e-w
			box := r_new
			cell_end := (*w)[box.r][box.c]

			for cell_end == BOXL || cell_end == BOXR {
				box = add(box, step)
				cell_end = (*w)[box.r][box.c]
			}
			if cell_end == WALL {
				return false
			}
			if cell_end == SPACE {
				for box != r_new {
					box_m := sub(box, step)
					(*w)[box_m.r][box_m.c], (*w)[box.r][box.c] = (*w)[box.r][box.c], (*w)[box_m.r][box_m.c]
					box = box_m
				}
				*r = r_new
				return true
			}
		} else { // move n-s
			to_move := make(AH.Set[Pos])
			active := make(AH.Set[Pos])
			to_move[r_new] = true
			active[r_new] = true

			if cell == BOXL {
				to_move[add(r_new, Pos{0, 1})] = true
				active[add(r_new, Pos{0, 1})] = true
			} else {
				to_move[sub(r_new, Pos{0, 1})] = true
				active[sub(r_new, Pos{0, 1})] = true
			}

			blocked := false
			for !blocked && len(active) > 0 {
				next_active := make(AH.Set[Pos])
				for p := range active {
					pp := add(p, step)
					cell_ns := (*w)[pp.r][pp.c]

					if cell_ns == WALL {
						blocked = true
						break
					}

					if cell_ns == SPACE {
						continue
					}

					to_move[pp] = true
					next_active[pp] = true

					if cell_ns == BOXL {
						to_move[add(pp, Pos{0, 1})] = true
						next_active[add(pp, Pos{0, 1})] = true
					} else {
						to_move[sub(pp, Pos{0, 1})] = true
						next_active[sub(pp, Pos{0, 1})] = true
					}
				}

				active = next_active
			}
			if blocked {
				return false
			}

			move_ordered := []Pos{}
			for p := range to_move {
				move_ordered = append(move_ordered, p)
			}

			sort.Slice(move_ordered, func(i, j int) bool {
				if move_ordered[i].r != move_ordered[j].r {
					return move_ordered[i].r < move_ordered[j].r
				}

				return (move_ordered[i].c < move_ordered[j].c)
			})
			if c == 'v' {
				slices.Reverse(move_ordered)
			}

			for _, box := range move_ordered {
				box_m := add(box, step)
				(*w)[box_m.r][box_m.c], (*w)[box.r][box.c] = (*w)[box.r][box.c], (*w)[box_m.r][box_m.c]
			}
			*r = r_new

			return true
		}
	}
	return false
}

func gps(w Warehouse) int {
	score := 0
	for r_i, row := range w {
		for c_i, cell := range row {
			if cell == BOX || cell == BOXL {
				score += 100*r_i + c_i
			}
		}
	}

	return score
}

func Run() {
	ss, _ := AH.ParseLineGroups("../input/input15.txt", "|")
	w, r := parseWarehouse(ss[0], true)
	for _, c := range ss[1] {
		moveRobot(&r, &w, c)
	}
	p1 := gps(w)

	w, r = parseWarehouse(ss[0], false)
	for _, c := range ss[1] {
		moveRobot(&r, &w, c)
	}
	p2 := gps(w)

	AH.PrintSoln(15, p1, p2)
}
