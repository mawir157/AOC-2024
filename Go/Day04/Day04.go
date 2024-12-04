//go:build d04
// +build d04

package Day04

import (
	AH "AoC2024/adventhelper"
)

type Offsets struct {
	row_s int
	row_e int
	col_s int
	col_e int
	row_i int
	col_i int
}

func findWord2(grid []string, target string) (counter int) {
	revTarget := AH.ReverseString(target)
	rows := len(grid)
	cols := len(grid[0])
	lent := len(target)

	counter = 0

	offs := []Offsets{
		Offsets{0, rows, 0, cols - lent + 1, 0, 1},
		Offsets{0, rows - lent + 1, 0, cols, 1, 0},
		Offsets{0, rows - lent + 1, 0, cols - lent + 1, 1, 1},
		Offsets{0, rows - lent + 1, lent - 1, cols, 1, -1},
	}

	for _, off := range offs {
		for r := off.row_s; r < off.row_e; r++ {
			for c := off.col_s; c < off.col_e; c++ {
				ok_pos := true
				ok_neg := true
				for idx := 0; idx < lent; idx++ {
					rn := grid[r+off.row_i*idx][c+off.col_i*idx]
					ok_pos = ok_pos && (rn == target[idx])
					ok_neg = ok_neg && (rn == revTarget[idx])

					if !(ok_neg || ok_pos) {
						break
					}
				}
				if ok_pos {
					counter++
				}
				if ok_neg {
					counter++
				}
			}
		}
	}

	return
}

func findX(grid []string) (counter int) {
	rows := len(grid)
	cols := len(grid[0])
	counter = 0

	for r := 1; r < rows-1; r++ {
		for c := 1; c < cols-1; c++ {
			if grid[r][c] != 'A' {
				continue
			}

			if (((grid[r-1][c-1] == 'M') && (grid[r+1][c+1] == 'S')) ||
				((grid[r-1][c-1] == 'S') && (grid[r+1][c+1] == 'M'))) &&
				(((grid[r-1][c+1] == 'M') && (grid[r+1][c-1] == 'S')) ||
					((grid[r-1][c+1] == 'S') && (grid[r+1][c-1] == 'M'))) {
				counter++
			}

		}
	}

	return
}

func Run() {
	g, _ := AH.ReadStrFile("../input/input04.txt")

	part1, part2 := findWord2(g, "XMAS"), findX(g)

	AH.PrintSoln(1, part1, part2)

	return
}
