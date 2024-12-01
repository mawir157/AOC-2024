package adventhelper

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
)

// This func must be Exported, Capitalized, and comment added.
// Print the solution in a standardised format.
func PrintSoln(day int, soln1 interface{}, soln2 interface{}) {
	fmt.Println("Day", day)
	fmt.Println("  Part 1:", soln1)
	fmt.Println("  Part 2:", soln2)
}

// //////////////////////////////////////////////////////////////////////////////
func ReadStrFile(fname string) (strs []string, err error) {
	b, err := os.ReadFile(fname)
	if err != nil {
		return nil, err
	}

	lines := strings.Split(string(b), "\n")
	for _, l := range lines {
		// Empty line occurs at the end of the file when we use Split.
		if len(l) == 0 {
			continue
		}
		strs = append(strs, l)
	}

	return strs, nil
}

// Read a file to an array of integers.
func ReadIntFile(fname string) (nums []int, err error) {
	b, err := os.ReadFile(fname)
	if err != nil {
		return nil, err
	}

	lines := strings.Split(string(b), "\n")
	// Assign cap to avoid resize on every append.
	nums = make([]int, 0, len(lines))

	for _, l := range lines {
		// Empty line occurs at the end of the file when we use Split.
		if len(l) == 0 {
			continue
		}
		// Atoi better suits the job when we know exactly what we're dealing
		// with. Scanf is the more general option.
		n, err := strconv.Atoi(l)
		if err != nil {
			return nil, err
		}
		nums = append(nums, n)
	}

	return nums, nil
}

// combine groups of lines separate by empty lines
func ParseLineGroups(fname string, sep string) (strs []string, err error) {
	b, err := os.ReadFile(fname)
	if err != nil {
		return nil, err
	}

	lines := strings.Split(string(b), "\n")
	temp := ""
	for _, l := range lines {
		if l != "" {
			if len(temp) == 0 {
				temp = l
			} else {
				temp = temp + sep + l
			}
		} else {
			strs = append(strs, temp)
			temp = ""
		}
	}
	return strs, nil
}

// //////////////////////////////////////////////////////////////////////////////
// a^b
func PowInt(a int, b int) (n int) {
	n = 1
	for i := 0; i < b; i++ {
		n *= a
	}
	return
}

// returns the maximum and minimum of an array of ints
func MaxAndMin(arr []int) (max int, min int) {
	max, min = arr[0], arr[0]

	for _, i := range arr {
		if i > max {
			max = i
		}

		if i < min {
			min = i
		}
	}
	return
}

// returns the index of the maximum and minimum of an array of ints
func MaxAndMinIdx(arr []int) (maxIdx int, minIdx int) {
	max, min := arr[0], arr[0]
	maxIdx, minIdx = 0, 0

	for idx, i := range arr {
		if i > max {
			max = i
			maxIdx = idx
		}

		if i < min {
			min = i
			minIdx = idx
		}
	}
	return
}

// Head
func FirstRune(str string) (r rune) {
	for _, r = range str {
		return
	}
	return
}

// Last
func FinalRune(str string) (r rune) {
	for _, r = range str {
	}
	return
}

// RuneAr
func RuneAt(str string, n int) (r rune) {
	i := 0
	for i, r = range str {
		if i == n {
			return
		}
	}
	return
}

func SetRuneAt(s string, r rune, n int) string {
	return s[:n] + string(r) + s[n:]
}

// Tail
func TrimFirstRune(s string) string {
	rs := []rune(s)
	return string(rs[1:])
}

// Init
func TrimLastRune(s string) string {
	l := len(s)
	rs := []rune(s)
	return string(rs[:(l - 1)])
}

// Drop
func Drop(s string, n int) string {
	rs := []rune(s)
	return string(rs[n:])
}

// Take
func Take(s string, n int) string {
	rs := []rune(s)
	return string(rs[:n])
}

func TakeWhileDigit(s string) string {
	rs := []rune{}
	for _, c := range s {
		if unicode.IsDigit(c) {
			rs = append(rs, c)
		} else {
			break
		}
	}

	return string(rs)
}

// Reverse A String!
func ReverseString(s string) string {
	n := len(s)
	runes := make([]rune, n)
	for _, rune := range s {
		n--
		runes[n] = rune
	}
	return string(runes[n:])
}

// Not defined in math!
func AbsInt(i int) int {
	if i < 0 {
		return -i
	}
	return i
}

// Not defined in math!
func Sign(i int) int {
	if i == 0 {
		return 0
	} else if i < 0 {
		return -1
	}
	return 1
}

// Sets the bit at pos in the integer n.
func SetBit(n int, pos uint) int {
	n |= (1 << pos)
	return n
}

// Sets the bit at pos in the integer n.
func ClearBit(n int, pos uint) int {
	n &^= (1 << pos)
	return n
}

// returns integer array of all values from max to min inclusie
func MakeRange(min, max int) []int {
	a := make([]int, max-min+1)
	for i := range a {
		a[i] = min + i
	}
	return a
}

func Concat(arr1, arr2 []int) (out []int) {
	out = make([]int, len(arr1)+len(arr2))
	out = append(out, arr1[:]...)
	out = append(out, arr2[:]...)

	return
}

func Min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func Max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func MulDiv(a, b, c int) int {
	aDivC := a / c
	bDivC := b / c
	aModC := a % c
	bModC := b % c

	return (aDivC * bDivC * c) + (aDivC*bModC + aModC*bDivC) + ((aModC * bModC) / c)
}

func GCD(a, b int) int {
	for b != 0 {
		t := b
		b = a % b
		a = t
	}
	return a
}

func LCM(a, b int, integers ...int) int {
	result := a * b / GCD(a, b)

	for i := 0; i < len(integers); i++ {
		result = LCM(result, integers[i])
	}

	return result
}
