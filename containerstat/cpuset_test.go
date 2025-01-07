package main

import (
	"gotest.tools/v3/assert"
	"slices"
	"testing"
)

func TestMakeRangeInts(t *testing.T) {
	var tc = []struct {
		a, b int
		want []int
	}{
		{0, 0, []int{0}},
		{1, 0, []int{}},
		{2, 3, []int{2, 3}},
		{0, 9, []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}},
	}
	for _, tt := range tc {
		assert.Assert(t, slices.Equal(tt.want, makeRangeInts(tt.a, tt.b)), "test case: %v", tt)
	}
	assert.Assert(t, len(makeRangeInts(10, 99)) == 99-10+1, "test case: long list 10,99")

}

func genericTestParseCpuSetTerm(t *testing.T, parser func(string) ([]int, error)) {
	// func TestParseCpuSetTerm(t *testing.T) {
	// var emptySet = []int{}
	var cases = []struct {
		s    string
		want []int
	}{
		{"7", []int{7}},
		{"77", []int{77}},
		{"0-3", []int{0, 1, 2, 3}},
	}
	var failCases = []struct {
		s string
	}{
		{""},
		{"x"},
		{" "},
		{"9999"},
	}
	for _, tt := range cases {
		r, e := parser(tt.s)
		es := ""
		if e != nil {
			es = e.Error()
		}
		assert.Assert(t, e == nil, "test case: %v (%s)", tt, es)
		assert.Assert(t, slices.Equal(tt.want, r))
	}
	for _, tt := range failCases {
		_, e := parser(tt.s)
		assert.Assert(t, e != nil, "test case: ", tt.s)
	}
}

func TestParseCpuSetTermG(t *testing.T) { genericTestParseCpuSetTerm(t, parseCpuSetTerm) }
func TestParseCpuSetG(t *testing.T)     { genericTestParseCpuSetTerm(t, parseCpuSet) }

func TestParseCpuSetExtendedCases(t *testing.T) {
	var cases = []struct {
		s    string
		want []int
	}{
		{"0,1", []int{0, 1}},
		{"0,1,17", []int{0, 1, 17}},
		{"0-3,4", []int{0, 1, 2, 3, 4}},
		{"0-3,4,5", []int{0, 1, 2, 3, 4, 5}},
		{"0-3,4,9-13", []int{0, 1, 2, 3, 4, 9, 10, 11, 12, 13}},
		{"5-9,0-3", []int{0, 1, 2, 3, 5, 6, 7, 8, 9}},
	}

	for _, tt := range cases {
		r, e := parseCpuSet(tt.s)
		assert.Assert(t, e == nil, "test case: %v (%s)", tt, ew(e))
		assert.Assert(t, slices.Equal(tt.want, r), "test case: %v != %v (%s)", tt, r, ew(e))
	}
}

func ew(err error) string {
	if err == nil {
		return "No Error"
	} else {
		return err.Error()
	}
}
