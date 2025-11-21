//go:build d25
// +build d25

package Day25

import (
	AH "AoC2024/adventhelper"
	"fmt"
)

type KeyLock [5]int

func parseKeysAndLock(ss []string) ([]KeyLock, []KeyLock) {
	keys := []KeyLock{}
	locks := []KeyLock{}

	timKey := KeyLock{-1, -1, -1, -1, -1}
	isLock := true
	for is, s := range ss {
		if is != 0 && is%7 == 0 {
			if isLock {
				locks = append(locks, timKey)
			} else {
				keys = append(keys, timKey)
			}
			timKey = KeyLock{-1, -1, -1, -1, -1}
			isLock = true
		}

		for i, rn := range s {
			if rn == '#' {
				timKey[i]++
			} else {
				if is%7 == 0 {
					isLock = false
				}
			}
		}
	}

	if isLock {
		locks = append(locks, timKey)
	} else {
		keys = append(keys, timKey)
	}

	return keys, locks
}

func youCompleteMe(ks []KeyLock, ls []KeyLock) int {
	count := 0
	fmt.Println(len(ks), len(ls))
	for _, k := range ks {
		for _, l := range ls {
			aPerfectFit := true
			for pin := 0; pin < 5; pin++ {
				aPerfectFit = aPerfectFit && (k[pin]+l[pin] <= 5)
			}
			if aPerfectFit {
				count++
			}
		}
	}

	return count
}

func Run() {
	ss, _ := AH.ReadStrFile("../input/input25.txt")
	ks, ls := parseKeysAndLock(ss)

	AH.PrintSoln(9, youCompleteMe(ks, ls), "What will survive of us is love")
}
