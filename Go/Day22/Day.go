//go:build d22
// +build d22

package Day22

import (
	AH "AoC2024/adventhelper"
	"strconv"
)

func hash(v0, v1, v2, v3 int) int {
	hash := 20*20*20*(v3+9) +
		20*20*(v2+9) +
		20*(v1+9) +
		(v0 + 9)
	return hash
}

func rng(seed int, items int) ([]int, map[int]int) {
	secret := seed
	random := []int{secret}
	diffs := []int{}

	for i := 0; i < items; i++ {
		secret = secret ^ (secret << 6)
		secret &= 0xffffff
		secret = secret ^ (secret >> 5)
		secret &= 0xffffff
		secret = secret ^ (secret << 11)
		secret &= 0xffffff

		random = append(random, secret)
		diffs = append(diffs, random[i+1]%10-random[i]%10)
	}

	bananas := make(map[int]int)
	for i := 0; i < len(random)-5; i++ {
		h := hash(diffs[i], diffs[i+1], diffs[i+2], diffs[i+3])
		if _, ok := bananas[h]; !ok {
			bananas[h] = random[i+4] % 10
		}
	}

	return random, bananas
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input22.txt")

	prices := make(map[int]int)
	p1 := 0
	p2 := 0
	for _, s := range ss {
		v, _ := strconv.Atoi(s)
		seq, bps := rng(v, 2000)
		p1 += seq[len(seq)-1]

		for k, v := range bps {
			prices[k] += v
		}
	}

	for _, v := range prices {
		if v > p2 {
			p2 = v
		}
	}

	AH.PrintSoln(22, p1, p2)

	return
}
