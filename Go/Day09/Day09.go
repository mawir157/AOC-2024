//go:build d09
// +build d09

package Day09

import (
	AH "AoC2024/adventhelper"
	"sort"
)

type File struct {
	s, l, i int
}

func parseInput(s string, part1 bool) ([]File, []File) {
	fs := []File{}
	gs := []File{}

	write := true
	id := 0
	ptr := 0

	for _, c := range s {
		sz := int(c - '0')
		if sz != 0 {
			if write {
				if part1 {
					for i := 0; i < sz; i++ {
						fs = append(fs, File{ptr + i, 1, id})
					}
				} else {
					fs = append(fs, File{ptr, sz, id})
				}
				id++
			} else {
				gs = append(gs, File{ptr, sz, -1})
			}
		}
		ptr += sz
		write = !write
	}

	return fs, gs
}

func defrag(fs []File, gs []File, part1 bool) []File {
	for to_move := len(fs) - 1; to_move >= 0; to_move-- {
		for fi := len(fs) - 1; fi >= 0; fi-- {
			f := fs[fi]
			moved := false
			if !part1 {
				if f.i != to_move {
					continue
				}
			}

			for gi, gap := range gs {
				if gap.s > f.s {
					continue
				}
				if f.l <= gap.l {
					gs = append(gs, File{f.s, f.l, -1})
					fs[fi].s = gap.s
					if f.l == gap.l {
						gs = append(gs[:gi], gs[gi+1:]...)
					} else {
						gs[gi].l -= f.l
						gs[gi].s += f.l
					}
					moved = true
					break
				}
			}

			if moved {
				if !part1 {
					sort.Slice(fs, func(i, j int) bool {
						return fs[i].s < fs[j].s
					})
				}

				ngs := []File{}
				curse := gs[0].s
				curl := 0
				for _, g := range gs {
					if curse+curl == g.s {
						curl += g.l
					} else {
						ngs = append(ngs, File{curse, curl, -1})
						curse = g.s
						curl = g.l
					}
				}
				ngs = append(ngs, File{curse, curl, -1})
				gs = ngs
			}
		}
		if part1 {
			to_move = 0
		}
	}

	return fs
}

func checkSum(fs []File) int {
	sum := 0

	for _, f := range fs {
		sum += f.i * f.l * (2*f.s + f.l - 1) / 2
	}

	return sum
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input09.txt")
	fs, gs := parseInput(ss[0], true)
	ffs := defrag(fs, gs, true)
	p1 := checkSum(ffs)
	fs, gs = parseInput(ss[0], false)
	ffs = defrag(fs, gs, false)
	p2 := checkSum(ffs)

	AH.PrintSoln(9, p1, p2)

	return
}
