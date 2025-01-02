package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

var regexStatLine0 = regexp.MustCompile(`^cpu (?: [0-9]{1,10}){10}$`)

func processProcStatLine0(s string) error {
	if regexStatLine0.FindStringIndex(s) == nil {
		return fmt.Errorf("proc stat line 0 failed to match")
	} else {
		return nil
	}
}

// func processProcStatLineN()
// regex used twice, once to confirm structure once to extract the repeated group ints.
// single regex would work, but the regex would need to be written out in full with ~11 capture groups,
// and need rewriting for a different specification of counters.

var regexStatLineFormat = regexp.MustCompile(`^cpu([0-9]{1,3})(?: [0-9]{1,10}){10}$`)
var regexStatLineAll = regexp.MustCompile(` ([0-9]{1,10})`)

func processProcStatLineN(s string) (uint8, [expectedTickCounters]uint32, error) {
	matchFormat := regexStatLineFormat.FindStringSubmatch(s)
	if len(matchFormat) == 0 {
		return 0, [expectedTickCounters]uint32{}, fmt.Errorf("proc stat line N failed to match format")
	} else if submatches := regexStatLineAll.FindAllStringSubmatch(s, -1); len(submatches) != 10 {
		return 0, [expectedTickCounters]uint32{}, fmt.Errorf("proc stat line N wrong count %d", len(submatches))
	} else {
		cpu, _ := strconv.Atoi(matchFormat[1])
		var ix [expectedTickCounters]uint32
		for i := range submatches {
			n, _ := strconv.Atoi(submatches[i][1])
			ix[i] = uint32(n)
		}
		return uint8(cpu), ix, nil
	}
}

var regexStatLineIntr = regexp.MustCompile(`^intr(?: [0-9]{1,10})+$`)

func processProcStatLineIntr(s string) error {
	if regexStatLineIntr.FindStringIndex(s) == nil {
		return fmt.Errorf("proc stat line intr failed to match")
	} else {
		return nil
	}
}
func ProcessProcStatInfo(infos []string) ([][expectedTickCounters]uint32, error) {
	// input is the line delineated content of /proc/stat
	// the function extracts the lines of form cpuXX, checking the prior and terminating line format as it goes
	// The return value is the matrix converted-to-integer form of the per-cpu counter vector
	// with n(CPU) rows and 'expectedTickCounters' columns.
	var tickCounters [][expectedTickCounters]uint32

	if len(infos) < 9 {
		return [][expectedTickCounters]uint32{}, fmt.Errorf("ProcessProcStatInfo: not enough lines from /proc/stat")
	} else if err := processProcStatLine0(infos[0]); err != nil {
		return [][expectedTickCounters]uint32{}, err
	} else {
		var i int
		for i = 1; i < len(infos)-expectedExtraLinesInProcStat; i++ {
			if cpu, counters, err := processProcStatLineN(infos[i]); err != nil {
				// fmt.Fprintf(os.Stderr, "processProcStatLineN() error at %d in %s\n", i, infos[i])
				return [][expectedTickCounters]uint32{}, err
			} else if cpu != uint8(i-1) {
				return [][expectedTickCounters]uint32{}, fmt.Errorf("ProcessProcStatInfo: unexpected CPU index")
			} else {
				tickCounters = append(tickCounters, counters)
			}
		}
		if err := processProcStatLineIntr(infos[i]); err != nil {
			// fmt.Fprintf(os.Stderr, "processProcStatLineIntr() error at %d in %s\n", i+1, infos[i+1])
			return [][expectedTickCounters]uint32{}, err
		} else {
			return tickCounters, nil
		}
	}
}

func nCpusFromProcStatInfo() (rVal int) {
	if data, err := ProcessProcStatInfo(getProcStat()); err != nil {
		fmt.Fprintf(os.Stderr, "error %s reading ncpus from /proc/stat\n", err.Error())
	} else {
		rVal = len(data)
	}
	return
}

func getProcStat() (infos []string) {
	if procFile, err := os.Open("/proc/stat"); err != nil {
		fmt.Fprintln(os.Stderr, "unexpected error reading /proc/stat")
		os.Exit(1)
	} else {
		scanner := bufio.NewScanner(procFile)

		for scanner.Scan() {
			infos = append(infos, scanner.Text())
		}

		if err := scanner.Err(); err != nil {
			fmt.Fprintln(os.Stderr, "scan error:", err)
			os.Exit(1)
		}
	}
	return
}

func GetProcessProcStatInfo() ([][expectedTickCounters]uint32, error) {
	return ProcessProcStatInfo(getProcStat())
}
