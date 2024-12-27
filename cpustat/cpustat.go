package main

import (
	"bufio"
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
	expectedTickCounters = 10
	tickIntervalMs = 500
)

var (
	cpuCountDetected int
	dataSeries       map[string][][expectedTickCounters]uint32
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
					// ignore summary line,which incidentally is not well formatted (double space between forts two words)
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

func processProcStatInfo(infos [][]string) {

	firstRun := dataSeries == nil

	if firstRun {
		cpuCountDetected = len(infos)
		dataSeries = map[string][][expectedTickCounters]uint32{}
	} else {
		if cpuCountDetected != len(infos) {
			fmt.Fprintln(os.Stderr, "inconsistent CPU count error:")
			fmt.Fprintf(os.Stderr, "%d != %d\n", cpuCountDetected, len(infos))
			os.Exit(1)
		}
	}

	for _, info := range infos {
		var sample [expectedTickCounters]uint32
		for i, item := range info[1:] {
			if n, err := strconv.Atoi(item); err != nil {
				fmt.Fprintln(os.Stderr, "atoi failed:%s", item)
				os.Exit(1)
			} else {
				sample[i] = uint32(n)
			}
		}
		key := info[0]

		if firstRun {
			dataSeries[key] = [][expectedTickCounters]uint32{sample}
		} else if row, present := dataSeries[key]; present {
			row = append(row, sample)
			dataSeries[key] = row
		} else {
			fmt.Fprintf(os.Stderr, "unexpected CPU id error:%s\n", key)
			os.Exit(1)
		}
	}
}

func main() {
	fmt.Println("hello word")
	ticker := time.NewTicker(tickIntervalMs * time.Millisecond)

	sigc := make(chan os.Signal, 1)
	signal.Notify(sigc, syscall.SIGINT)

selectLoop:
	for {
		select {
		case _ = <-sigc:
			break selectLoop
		case _ = <-ticker.C:
			fmt.Fprint(os.Stderr, ".")
			doTickAction()
		}
	}
	cpuCount := len(dataSeries)
	cpuIds := maps.Keys(dataSeries)
	aRandomSample := dataSeries[cpuIds[0]]
	sampleCount := len(aRandomSample)
	fmt.Fprintf(os.Stderr, "\nexiting with %d cpus monitored, %d samples\n", cpuCount, sampleCount)
}
