package main

/*
cpuset - parse set of CPUs, identified by index, to be monitored and reported

Q0: how to parse a string like 21,22,17-45
Q1: how to represent a string like that

Q1 - the meaning is a set of small integers
subject to issues of ordering and duplication, such as set can be a slice of ints.
So Let It Be
Q0:
top level terms are separated by ','
a term may be either <int> or <int>-<int>.
A parser which returns [int] can accept either form.
It is so naturally a regex, it would be wrong not to make it one.

*/

import (
	"fmt"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

var simpleTerm = regexp.MustCompile(`^[0-9]{1,3}$`)
var rangeTerm = regexp.MustCompile(`^([0-9]{1,3})-([0-9]{1,3})$`)

func parseCpuSet(s string) (set []int, _ error) {
	// probably a fatter regex could do the whole process in one step, however....
	terms := strings.Split(s, ",")
	for _, term := range terms {
		if setx, err := parseCpuSetTerm(term); err != nil {
			return nil, err
		} else {
			set = append(set, setx...)
		}
	}
	slices.Sort(set)
	return
}

func makeRangeInts(a, b int) []int {
	if a > b {
		return []int{}
	} else if a == b {
		return []int{a}
	} else {
		return append([]int{a}, makeRangeInts(a+1, b)...)
	}
}

func parseCpuSetTerm(s string) ([]int, error) {
	rangeMatch := rangeTerm.FindStringSubmatch(s)
	if rangeMatch != nil && len(rangeMatch) == 3 {
		// no need to check atoi() success because regex guarantees correctness
		n0, _ := strconv.Atoi(rangeMatch[1])
		n1, _ := strconv.Atoi(rangeMatch[2])
		return makeRangeInts(n0, n1), nil

	}

	simpleMatch := simpleTerm.FindString(s)
	if simpleMatch == "" {
		return nil, fmt.Errorf("invalid empty string in term")
	} else {
		// no need to check atoi() success because regex guarantees correctness
		n, _ := strconv.Atoi(simpleMatch)
		return []int{n}, nil
	}
}

func isSubSetOf(a, b []int) bool {
	return true
}
