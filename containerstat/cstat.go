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
		timestamp:      time.Now().UnixMicro(),
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
