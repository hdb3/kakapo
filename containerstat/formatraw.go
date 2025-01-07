package main

import (
	"fmt"
	"github.com/google/uuid"
	"io"
	"os"
	"time"
)

// 	dataSeries       map[int][][expectedTickCounters]uint32
/*
save raw data with no transformation
data structure is map[int][][expectedTickCounters]uint32
where the map key is the CPU ID, and the values are a time series of vectors,
each vector being the set of incrementing total of tick counts for each category of CPU usage.
There are currently 10 counters, the list below shows most of them.
It is expected that the sum of all counters is simply the number of ticks (@100Hz) since the system started.
In the intended context of CPUs allocate dto a process or docker container most counters are expected to be zero, or very small.
Whether system ticks should be included is a choice for the user.
If the intent is to evaluate an applications utilisation of multiple CPUs it should make no difference,
since kernel activity is not possible on behalf of unrelated applications.
A simple aggregation function sums expected use counters - 1-3 , the idle counter - 4, and the remaining expected zero or low counters.
for these classes:
application - 1-3
idle - 4
other - 5-10(?)
Where other is expected to be < 1%.
A completely raw data stream does not perform this aggregation.
A general purpose datastream transformation performs the aggregation and reports if the 'other' category exceeds the expected
level.  The same transformation provides the delta calculation, so that the resulting data can be directly graphed with some expectation of actual visible meaning.
This form is called 'normalised'.  It has N x 1rows: where row 0 is timestamp, and row n is the 'busy' count for the CPU corresponding to the row.

CPU ID reporting in output
Since the only datasets used are either inclusive of all CPUs in the system, or selected explicitly on the capture command,
and the actual CPU identity changes arbitrarily between runs, the preservation of CPU identity is superfluous.
Excluding CPU identity simplifies data format with no loss of information.
*/

/*
raw format

raw format is a line delineated time series report in which each sample is a matrix indexed by cpu id and tick counter category
Since dropping or amalgamating counter classes is an expected transformation,before dropping or transforming CPU granularity,
it is better to make the inner index in serialised form the counter class rather than the CPU index
*/
// func Fprintf(w io.Writer, format string, a ...any) (n int, err error)

// func formatRawHeaderLine(writer io.Writer,key string, value string){
const formatName = "hdb3.net/cpustat"
const shortFormatName = "cpustat"
const formatVersion = "1.0"
const formatDescription = "timeseries data of CPU and memory usage for a docker container"

var formatRawHeaderStartTime time.Time

func init()                { formatRawHeaderInit() }
func formatRawHeaderInit() { formatRawHeaderStartTime = time.Now() }
func formatRawHeaderLine(writer io.Writer, key string, format string, a ...any) {
	fmt.Fprintf(writer, "#%s: ", key)
	fmt.Fprintf(writer, format, a...)
	fmt.Fprintln(writer)

}

func formatRawFileName() string {
	return fmt.Sprintf("%s-%d.data", shortFormatName, formatRawHeaderStartTime.Unix())
}

func humanFormatTimestamp(t time.Time) string {
	micros := t.UnixMicro() - t.Unix()*1000000
	dateTime := t.Format(time.DateTime)
	return fmt.Sprintf("%s.%06d", dateTime, micros)
}

func formatTimestamp(t time.Time) string {
	return formatTimestamp64(t.UnixMicro())
}

func formatTimestamp64(i64 int64) string {
	return fmt.Sprintf("%8.06f", float64(i64)/1000000.0)
}

func formatRawHeader(writer io.Writer, now time.Time) {
	// now := time.Now()
	hostname, _ := os.Hostname()
	uuid := uuid.New()
	formatRawHeaderLine(writer, "filetype", "TSV data")
	formatRawHeaderLine(writer, "format", formatName)
	formatRawHeaderLine(writer, "version", formatVersion)
	formatRawHeaderLine(writer, "description", formatDescription)
	formatRawHeaderLine(writer, "create", "%s", formatTimestamp(formatRawHeaderStartTime))
	formatRawHeaderLine(writer, "createx", "%s", humanFormatTimestamp(formatRawHeaderStartTime))
	formatRawHeaderLine(writer, "hostname", "%s", hostname)
	formatRawHeaderLine(writer, "uuid", "%s", uuid)
	if externalUUID != nil {
		formatRawHeaderLine(writer, "xuuid", "%s", externalUUID)
	}
}

func formatRawTrailer(writer io.Writer, now time.Time) {

	formatRawHeaderLine(writer, "end", "%s", formatTimestamp(now))
	formatRawHeaderLine(writer, "endx", "%s", humanFormatTimestamp(now)) // TODO experiment with dropping the "%s"
	duration := now.Sub(formatRawHeaderStartTime)
	formatRawHeaderLine(writer, "duration", "%s", duration)
}

/*
credit: https://www.idnt.net/en-US/kb/941772
somewhere in linux kernel source is the source for this...

Column	Name	Description	Kernel
1	user	Time spent with normal processing in user mode.
2	nice	Time spent with niced processes in user mode.
3	system	Time spent running in kernel mode.
4	idle	Time spent in vacations twiddling thumbs.
5	iowait	Time spent waiting for I/O to completed. This is considered idle time too.
6	irq	Time spent serving hardware interrupts. See the description of the intr line for more details.
7	softirq	Time spent serving software interrupts.
8	steal	Time stolen by other operating systems running in a virtual environment.
9	guest	Time spent for running a virtual CPU or guest OS under the control of the kernel.
*/
const (
	// all offsets are column numbers adjusted by one, because column zero in the source is the CPU ID, ~ key, which is removed from the raw data
	procUser   = 0
	procNice   = 1
	procSystem = 2
	procIdle   = 3
	procIoWait = 4
	procMax    = 9
)

/*
Specification for Data Files

- mandatory header lines
- - globally unique file format name
- - format version
- - creation time
- - creation time in human friendly format
- - system name
- - provided UUID (use for correlation with any other dataset, including multiple files from the same source)
- - internal UUID
- - column names
- - column types
- Mandatory finishing ‘comments’
- - end time
- - end time in human friendly format
- - (data) line count
- - duration/elapsed time
- Data format
- - TSV
- - strings not include TAB or LF, but can use \t, \n for them
- - no explicit line length limit
- - all columns should be consistent and correspond to column types - integral, float, string, enum
- - where used enum should be consistent (defined for that file format)
- - strings may be zero length
- - integral types may have null or string alternates, but only as defined for that format
- - all columns should be present
- - An implied unambiguous translation to JSON is always possible (since even header lines have keys).

*/
