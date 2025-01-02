package main

import (
	"gotest.tools/v3/assert"
	"testing"
	// "fmt"
	// "os"
)

func TestProcessProcStatInfo(t *testing.T) {
	data, err := ProcessProcStatInfo(getProcStat())
	assert.Assert(t, err == nil, "%s", ew(err))
	assert.Assert(t, len(data) > 0)
	assert.Assert(t, len(data) < 256)

	// // if `ncpus` == 20 this would also be valid...
	// fmt.Fprintf(os.Stderr, "%d\n", len(data))
	// assert.Assert(t, len(data) == 20)
}

func TestProcessProcStatLine0(t *testing.T) {

	err := processProcStatLine0(getProcStat()[0])
	assert.Assert(t, err == nil, "failed with \"%s\" : %s", getProcStat()[0], ew(err))
}

func TestProcessProcStatLineN(t *testing.T) {

	{
		_, _, err := processProcStatLineN(getProcStat()[1])
		assert.Assert(t, err == nil, "failed with \"%s\" : %s", getProcStat()[1], ew(err))
	}
	{
		line15 := "cpu15 324478 29 149053 37563358 28095 0 1377 0 0 0"
		cpu, counters, err := processProcStatLineN(line15)
		assert.Assert(t, err == nil, "failed with \"%s\" : %s", line15, ew(err))
		assert.Assert(t, cpu == 15, "cpu wrong, failed in \"%s\" : %d != 15", line15, cpu)
		assert.Assert(t, len(counters) == expectedTickCounters, "len(counters) wrong, failed in \"%s\" : %d != %d", line15, len(counters), expectedTickCounters)
		assert.Assert(t, counters[0] == 324478, "counters[0]==(%d) != 324478, failed in \"%s\" : %d != %d", counters[0], line15)
		assert.Assert(t, counters[4] == 28095, "counters[4]==(%d) != 28095, failed in \"%s\" : %d != %d", counters[4], line15)
	}
}
