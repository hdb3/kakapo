package main

import (
	"flag"
	"fmt"
	"os"
	"os/signal"
	"syscall"
	"time"
)

// constants constant for a given kernel implementation of /proc/stat
const (
	expectedTickCounters         = 10
	expectedExtraLinesInProcStat = 7
)

var (
	rawData         [][][expectedTickCounters]uint32
	rawDataBaseLine [][expectedTickCounters]uint32
	rawDataHeight   int
)

func doTickAction(cpuSet []int) {
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

		// perform delta translation at point of collection.....

		if rawDataBaseLine == nil {
			rawDataBaseLine = sample
		} else {
			for i := range sample {
				for j := range sample[i] {
					// should assert that sample[i][j] >= rawDataBaseLine[i][j]
					sample[i][j] -= rawDataBaseLine[i][j]
				}
			}
			rawData = append(rawData, sample)
		}
	}
}

func main() {
	var (
		cpuSet         []int
		tickIntervalMs int
		dockerMonitor  *dockerMonitor
	)

	flag.IntVar(&tickIntervalMs, "interval", 200, "sampling interval")
	flag.IntVar(&tickIntervalMs, "i", 200, "sampling interval")
	cpusetString := flag.String("cpuset", "", "CPUs to monitor")
	dockerDaemonName := flag.String("docker", "", "docker daemon to monitor")
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

	if *dockerDaemonName != "" {
		dockerMonitor = initDockerMonitor(*dockerDaemonName)
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
			doTickAction(cpuSet)
			if dockerMonitor != nil {
				dockerMonitor.doTickAction()
			}
			samples += 1
		}
	}

	fmt.Fprintf(os.Stderr, "\nexiting with %d cpus monitored, %d samples\n", len(cpuSet), len(rawData))
	fmt.Fprint(os.Stderr, "\n*** raw data***]\n", rawData, "\n\n")
}
