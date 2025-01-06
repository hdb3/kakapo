package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/google/uuid"
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

func formatRawDataItem(writer io.Writer, item *dataItem) {
	fmt.Fprint(writer, formatTimestamp64(item.timestamp))
	formatRawDataContainerStats(writer, &item.containerStats)
	formatRawDataCpuStats(writer, &item.cpuStats)
	fmt.Fprintln(writer)
}

func formatRawDataCpuStats(writer io.Writer, stats *cpuStats) {
	s := ""
	for _, stat := range stats.normalise() {
		s = s + fmt.Sprintf("\t%d", stat)
	}
	fmt.Fprint(writer, s)
}

func formatRawDataContainerStats(writer io.Writer, item *containerStats) {
	fmt.Fprint(writer, item.show())
}

/*
func (containerStats containerStats) hdr() string {
*/
type dataItem struct {
	timestamp int64
	containerStats
	cpuStats
}

type dataLogger struct {
	rawData []dataItem
}

func initDataLogger() dataLogger {
	return dataLogger{}
}
func (dataLogger *dataLogger) writeRawFile() {
	fname := formatRawFileName()
	if file, err := os.Create(fname); err != nil {
		fmt.Fprintf(os.Stderr, "failed to open log file %s: %s\n", fname, err.Error())
		os.Exit(1)
	} else {
		now := time.Now()
		writer := bufio.NewWriter(file)
		formatRawHeader(writer, now)
		dataLogger.formatRawData(writer)
		formatRawTrailer(writer, now)
		writer.Flush()
		file.Close()
	}

}

func (log *dataLogger) logItem(cpuStats cpuStats, memStats containerStats) {
	log.rawData = append(log.rawData, dataItem{
		timestamp:      time.Now().UnixMicro() / 1000000.0,
		cpuStats:       cpuStats,
		containerStats: memStats,
	})
}

func (log *dataLogger) formatRawData(writer io.Writer) {
	for _, item := range log.rawData {
		formatRawDataItem(writer, &item)
	}
}

var (
	externalUUID *uuid.UUID
)

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
	}
	return
}

func main() {
	var (
		tickIntervalMs int
		dockerMonitor  *dockerMonitor
	)

	flag.IntVar(&tickIntervalMs, "interval", 200, "sampling interval")
	flag.IntVar(&tickIntervalMs, "i", 200, "sampling interval")
	cpusetString := flag.String("cpuset", "", "CPUs to monitor")
	uuidString := flag.String("uuid", "", "external uuid to include in log file")
	dockerDaemonName := flag.String("docker", "", "docker daemon to monitor")
	flag.Parse()

	logger := initDataLogger()

	cpuStatCollector := cpuStatsInit(*cpusetString)

	if *uuidString != "" {
		if uuid, err := uuid.Parse(*uuidString); err != nil {
			fmt.Fprintf(os.Stderr, "invalid uuid: %s (%s)\n", *uuidString, err.Error())
			os.Exit(1)
		} else {
			externalUUID = &uuid
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
	ticks := 0
selectLoop:
	for {
		select {
		case _ = <-sigc:
			break selectLoop
		case _ = <-displayTicker.C:
			fmt.Fprintf(os.Stderr, "\r%d", ticks)
		case _ = <-actionTicker.C:
			if dockerMonitor != nil {
				memStats := dockerMonitor.doTickAction()
				logger.logItem(doTickAction(cpuStatCollector), *memStats)
			} else {
				logger.logItem(doTickAction(cpuStatCollector), containerStats{})
			}
		}
		ticks++
	}

	fmt.Fprintf(os.Stderr, "\nexiting with %d cpus monitored, %d samples\n", cpuStatCollector.cpuSetSize, len(logger.rawData))
	// fmt.Fprint(os.Stderr, "\n*** raw data***]\n", logger.rawData, "\n\n")
	logger.writeRawFile()
}
