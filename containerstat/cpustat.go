package main

import (
	"fmt"
	"os"
)

// these constants constant for a given kernel implementation of /proc/stat
const (
	expectedTickCounters         = 10
	expectedExtraLinesInProcStat = 7
)

type cpustat [expectedTickCounters]uint32

func intFloatDiv(a, b uint32) float32 {
	return float32(a) / float32(b)
}
func (cpustat cpustat) normalise() uint32 {
	var sum, busy, idle, other uint32
	for i, c := range cpustat {
		sum += c
		switch i {
		case procUser, procNice, procSystem:
			busy += c
		case procIdle, procIoWait:
			idle += c
		default:
			other += c
		}
	}
	otherRatio := intFloatDiv(sum-busy-idle, sum)
	if otherRatio > 0.1 {
		fmt.Fprintf(os.Stderr, "unreasonably high other ratio: %f (busy:%f, idle:%f, other:%f)\n", otherRatio, intFloatDiv(busy, sum), intFloatDiv(idle, sum), intFloatDiv(other, sum))
		fmt.Fprintf(os.Stderr, "                             : (%#v)\n", cpustat)
	}
	return busy
}

type cpuStats []cpustat

func (cpuStats *cpuStats) normalise() (s []uint32) {
	for _, cpustat := range *cpuStats {
		s = append(s, cpustat.normalise())
	}
	return
}

/*
cpuStats

cpuStats is the datatype which represents the CPU usage sampled for the duration of the application
It is a variable sized structure because the number of CPUs in a system, and the number of CPUs to be monitored, are only known at run time.
But, once known, they do not change for the duration of the program.
Secondly, the number of variables per CPU is somewhat flexible, unless the decision is to store and process more variables than needed through out the application operation.

The capture function must have access and control over these size variables, but the general processing logic should not.

The type 'cpuStats' represents this structure.

Because of this runtime variable structure, the state of the current system has to be held in some persistent state,
which should be initialised before regular statistics collection starts.
*/
type cpuStatsState struct {
	cpuSet         []int
	cpuSetSize     int
	baseLine       *cpuStats
	systemCpuCount int
}

func (cpuStatsState cpuStatsState) header() (hdr string) {
	for _, cpu := range cpuStatsState.cpuSet {
		hdr = hdr + fmt.Sprintf("\tcpu %d", cpu)
	}
	return
}

func cpuStatsInit(cpuSetConfigString string) *cpuStatsState {

	// the target CPU list is either the full list as shown by actually reading /proc/stat, or, some subset given by a command parameter string

	var requestedCpuSet []int
	var st cpuStatsState
	highestRequestedCpu := 0
	if cpuSetConfigString != "" {
		if cpuset, err := parseCpuSet(cpuSetConfigString); err != nil {
			fmt.Fprintf(os.Stderr, "invalid cpuset string: %s (%s)\n", cpuSetConfigString, err.Error())
			fmt.Fprintf(os.Stderr, "example: --cpuset=1,2,10-12\n")
			os.Exit(1)
		} else {
			requestedCpuSet = cpuset
			highestRequestedCpu = requestedCpuSet[len(requestedCpuSet)-1]

		}
	}
	if data, err := ProcessProcStatInfo(getProcStat()); err != nil {
		fmt.Fprintf(os.Stderr, "error %s reading ncpus from /proc/stat\n", err.Error())
	} else {
		st.systemCpuCount = len(data)
	}

	if requestedCpuSet == nil {
		for i := 0; i < st.systemCpuCount; i++ {
			st.cpuSet = append(st.cpuSet, i)
		}
	} else if highestRequestedCpu > st.systemCpuCount {
		fmt.Fprintf(os.Stderr, "invalid cpuset, requested CPU (%d) > ncpus (%d))\n", highestRequestedCpu, st.systemCpuCount)
		os.Exit(1)
	} else {
		st.cpuSet = requestedCpuSet
	}
	st.cpuSetSize = len(st.cpuSet)
	fmt.Fprintf(os.Stderr, "cpuStatsState: %#v\n", st)
	return &st
}

func doTickAction(st *cpuStatsState) (sample cpuStats) {

	if cpuUsage, err := GetProcessProcStatInfo(); err != nil {
		fmt.Fprintln(os.Stderr, "unexpected error reading /proc/stat")
		os.Exit(1)
	} else if len(cpuUsage) != st.systemCpuCount {
		fmt.Fprintf(os.Stderr, "ncpus in /proc/stat changed (%d -> %d)\n", len(cpuUsage), st.systemCpuCount)
		os.Exit(1)
	} else {
		var stats cpuStats
		for _, cpu := range st.cpuSet {
			stats = append(stats, cpuUsage[cpu])
		}

		if st.baseLine == nil {
			st.baseLine = &stats
		}
		// perform delta translation at point of collection.....

		for i := range stats {
			var row cpustat
			for j := range stats[i] {
				// could assert that stats[i][j] >= rawDataBaseLine[i][j]
				row[j] = stats[i][j] - (*st.baseLine)[i][j]
			}
			sample = append(sample, row)
		}
		st.baseLine = &stats
	}
	return
}
