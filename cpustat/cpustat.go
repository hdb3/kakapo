package main

import (
	"bufio"
	"flag"
	"fmt"
	"golang.org/x/exp/maps"
	"os"
	"os/signal"
	"strconv"
	"strings"
	"syscall"
	"time"
)

const (
	expectedTickCounters         = 10
	expectedExtraLinesInProcStat = 7
)

var (
	tickIntervalMs int
)

var (
	dataSeries    map[int][][expectedTickCounters]uint32
	cpuSet        []int
	rawData       [][][expectedTickCounters]uint32
	rawDataHeight int
)

func doTickAction() {
	if procFile, err := os.Open("/proc/stat"); err != nil {
		fmt.Fprintln(os.Stderr, "unexpected error reading /proc/stat")
		os.Exit(1)
	} else {
		scanner := bufio.NewScanner(procFile)
		var cpuStatLines [][]string

		for scanner.Scan() {

			words := strings.Split(scanner.Text(), " ")
			if key, found := strings.CutPrefix(words[0], "cpu"); found {

				if key == "" {
					// ignore summary line,which incidentally is not well formatted (double space between first two words)
				} else if len(words) != expectedTickCounters+1 {
					fmt.Fprintf(os.Stderr, "invalid format error:%v, %d != %d \n", words, len(words), expectedTickCounters+1)
					os.Exit(1)
				} else {
					words[0] = key
					cpuStatLines = append(cpuStatLines, words)
				}
			}
		}

		if err := scanner.Err(); err != nil {
			fmt.Fprintln(os.Stderr, "scan error:", err)
			os.Exit(1)
		} else {
			processProcStatInfo(cpuStatLines)
		}
	}
}

func doTickAction2(cpuSet []int) {
	if cpuUsage, err := GetProcessProcStatInfo(); err != nil {
		fmt.Fprintln(os.Stderr, "unexpected error reading /proc/stat")
		os.Exit(1)
	} else {
		if rawData == nil {
			rawDataHeight = len(cpuSet)
		} else if rawDataHeight != len(cpuSet) {
			fmt.Fprintf(os.Stderr, "ncpus in /proc/stat changed (%d -> %d)\n", rawDataHeight, len(cpuSet))
			os.Exit(1)
		}

		var sample [][expectedTickCounters]uint32
		for _, cpu := range cpuSet {
			sample = append(sample, cpuUsage[cpu])
		}
		rawData = append(rawData, sample)
	}
}

func matrixStringToInt(sxx [][]string) ([][expectedTickCounters]uint32, error) {

	var intMatrix [][expectedTickCounters]uint32

	for i := range sxx {
		var row [expectedTickCounters]uint32

		for j := range sxx[i] {
			if n, err := strconv.Atoi(sxx[i][j]); err != nil {
				return nil, fmt.Errorf("atoi failed at %d,%d, reading %s", i, j, sxx[i][j])
			} else if j > expectedTickCounters {
				return nil, fmt.Errorf("line too long at %d", i)
			} else if j == 0 {
				if n != i {
					return nil, fmt.Errorf("CPU index not in sequence at %d", i)
				}
			} else {
				row[j-1] = uint32(n)
			}
		}
		intMatrix = append(intMatrix, row)
	}
	return intMatrix, nil
}

func processProcStatInfo(infos [][]string) {

	firstRun := dataSeries == nil

	// sample data is rows of integers represented as strings
	// the first column is the CPU ID, all other columns are tick counters
	// in every case, conversion to integer form is required, so do that first
	if m, err := matrixStringToInt(infos); err != nil {
		fmt.Fprintf(os.Stderr, "matrixStringToInt failed:%s\n", err.Error())
		os.Exit(1)
	} else {
		// first, process the sample keys -
		// on first run, check that the requested set of cpus is consistent with actual
		// and, initialise the sample store
		if firstRun {
			// Now the first input sample is confirmed sane, check that the highest requested CPU index is less than or equal that in the set
			// since cpuSet is sorted, the highest is the last.
			// If cpuSet is empty skip the check and simply use the input sample keys as the CPU set.

			if len(cpuSet) > 0 {
				lastElement := cpuSet[len(cpuSet)-1]
				if lastElement > len(m)-1 {
					fmt.Fprintf(os.Stderr, "invalid CPU set, requested CPU not in /proc/stat\n")
					fmt.Fprintf(os.Stderr, "highest requested CPU is %d , highest available is %d\n", lastElement, len(m)-1)
					os.Exit(1)
				}
			} else {
				for i := range m {
					cpuSet = append(cpuSet, i)
				}
			}

			dataSeries = map[int][][expectedTickCounters]uint32{}
			for _, cpuId := range cpuSet {
				dataSeries[cpuId] = [][expectedTickCounters]uint32{}
			}
		}
		// now, first run or not, the dataSeries map is configured for the requested CPUs only
		// task now is - store only the samples wanted from the matrix
		// in this version, the matrix is complete, so selection is needed,
		// but, the matrix is guaranteed sorted, so simple.
		// In future, an array data series would be better perhaps...

		for _, cpuId := range cpuSet {
			dataSeries[cpuId] = append(dataSeries[cpuId], m[cpuId])
		}
	}
}

func main() {
	flag.IntVar(&tickIntervalMs, "interval", 200, "sampling interval")
	flag.IntVar(&tickIntervalMs, "i", 200, "sampling interval")
	cpusetString := flag.String("cpuset", "", "CPUs to monitor")
	flag.Parse()
	if *cpusetString != "" {
		if cpuset, err := parseCpuSet(*cpusetString); err != nil {
			fmt.Fprintf(os.Stderr, "invalid cpuset string: %s (%s)\n", *cpusetString, err.Error())
			fmt.Fprintf(os.Stderr, "example: --cpuset=1,2,10-12\n")
			os.Exit(1)
		} else if cpuCount := nCpusFromProcStatInfo(); cpuset[len(cpuset)-1] > cpuCount {
			fmt.Fprintf(os.Stderr, "invalid cpuset, requested CPU (%d) > ncpus (%d))\n", cpuset[len(cpuset)-1], cpuCount)
			os.Exit(1)
		} else {
			cpuSet = cpuset
		}
	}
	fmt.Println("cpustat")
	actionTicker := time.NewTicker(time.Duration(tickIntervalMs) * time.Millisecond)
	displayTicker := time.NewTicker(251 * time.Millisecond)

	sigc := make(chan os.Signal, 1)
	signal.Notify(sigc, syscall.SIGINT)
	samples := 0
selectLoop:
	for {
		select {
		case _ = <-sigc:
			break selectLoop
		case _ = <-displayTicker.C:
			fmt.Fprintf(os.Stderr, "\r%d", samples)
		case _ = <-actionTicker.C:
			doTickAction()
			doTickAction2(cpuSet)
			samples += 1
		}
	}
	cpuCount := len(dataSeries)
	cpuIds := maps.Keys(dataSeries)
	aRandomSample := dataSeries[cpuIds[0]]
	sampleCount := len(aRandomSample)
	fmt.Fprintf(os.Stderr, "\nexiting with %d cpus monitored, %d samples\n", cpuCount, sampleCount)
	fmt.Fprint(os.Stderr, "\n*** raw data***]\n", dataSeries)

	fmt.Fprintf(os.Stderr, "\nexiting with %d cpus monitored, %d samples\n", len(cpuSet), len(rawData))
	fmt.Fprint(os.Stderr, "\n*** raw data***]\n", rawData)
}
