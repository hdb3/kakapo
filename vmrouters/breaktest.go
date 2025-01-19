package main

import (
	"flag"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strings"
	"time"
)

const tLoop = 250 // milliSeconds, the  granularity for fping
var verbose bool

var ipv4Regex = regexp.MustCompile(`^[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}$`)

func isValidIpV4(s string) bool {
	return ipv4Regex.MatchString(s)
}

func fping(target string, retry, timeout int64) bool {
	sRetry := fmt.Sprintf("%d", retry)
	sTimeout := fmt.Sprintf("%d", timeout)
	// fmt.Fprintf(os.Stderr, "command is: fping --quiet --retry %s --timeout %s %s\n", sRetry, sTimeout, target)
	cmd := exec.Command("fping", "--quiet", "--retry", sRetry, "--timeout", sTimeout, target)
	var stdout, stderr strings.Builder
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	err := cmd.Run()
	if !verbose {
		return cmd.ProcessState.Success()
	} else {
		if err != nil {
			fmt.Fprintln(os.Stderr, "fping failed")
		} else {
			fmt.Fprintln(os.Stderr, "fping succeeded")
		}
		fmt.Fprintf(os.Stderr, "stdout: '%s'\n", stdout.String())
		fmt.Fprintf(os.Stderr, "stderr: '%s'\n", stderr.String())
		if cmd.ProcessState == nil {
			fmt.Fprintln(os.Stderr, "panic, cmd.ProcessState == nil")
			return false
		} else {
			fmt.Fprintf(os.Stderr, "exitCode: %d\n", cmd.ProcessState.ExitCode())
			fmt.Fprintf(os.Stderr, "Success: %t\n", cmd.ProcessState.Success())
			return cmd.ProcessState.Success()
		}
	}
}

func pingTillReply(ip string, timeout int64) bool {
	// return true if the event happened, i.e. ping succeeded
	//   - ping-till-reply(tTimeout): fping --retry=tTimeout/tLoop --timeout=tLoop
	return fping(ip, timeout/tLoop, tLoop)

}
func pingTillNoReply(ip string, timeout int64) bool {
	start := time.Now()

	for time.Since(start).Milliseconds() < timeout {
		if !fping(ip, 1, tLoop) {
			if verbose {
				fmt.Fprintln(os.Stderr, "pingTillNoReply - fping 'got no reply'")
			}
			return true
		} else {
			time.Sleep(tLoop * time.Millisecond)
		}
	}
	if verbose {
		fmt.Fprintln(os.Stderr, "pingTillNoReply - fping always 'got reply'")
	}
	return false
}

func core(target string, cConnect, cComplete int64) (tConnect, tDisrupt, tResume, tRedisrupt int64) {
	start := time.Now()
	elapsed := func() int64 {
		return time.Since(start).Milliseconds()
	}

	if !pingTillReply(target, cConnect) {
		fmt.Fprintf(os.Stderr, "failed to reach %s\n", target)
		return
	} else {
		tConnect = elapsed()
		fmt.Fprintf(os.Stderr, "reached %s\n", target)
	}

	if !pingTillNoReply(target, cComplete) {
		fmt.Fprintln(os.Stderr, "exit with no disruption")
		return
	} else {
		tDisrupt = elapsed()
		fmt.Fprintf(os.Stderr, "disrupted after %d mS\n", tDisrupt)
	}

	if !pingTillReply(target, cComplete) {
		fmt.Fprintln(os.Stderr, "exit with no resumption")
		return
	} else {
		tResume = elapsed()
		fmt.Fprintf(os.Stderr, "resumed after %d mS\n", tResume)
	}

	if !pingTillNoReply(target, cComplete) {
		return
	} else {
		tRedisrupt = elapsed()
		fmt.Fprintf(os.Stderr, "disrupted again after %d mS\n", tRedisrupt)
	}

	/*


		- ping-till-reply with timeout cConnect
		- - on timeout exit 0,0,0,0
		- - on response record time as tConnect

		- ping-till-no-reply with timeout cComplete
		- - on timeout exit tConnect,0,0,0
		- - on no-response record time as tDisrupt

		- ping ping-till-reply with timeout cComplete
		- - on timeout exit tConnect,tDisrupt,0,0
		- - on response record time as tResume
		- ping-till-no-reply with timeout cComplete
		- - on no-response exit tConnect,tDisrupt,tResume,0
		- - on no-response record time as tRedisrupt, exit tConnect,tDisrupt,tResume,tRedisrupt

	*/

	return
}

func main() {

	targetIP := flag.String("target", "", "remote IP address to reach")
	cConnect := flag.Int64("connect", 1000, "max wait for initial connection (mS, default 1000)")
	cComplete := flag.Int64("complete", 5000, "max complete execution time (mS, default 5000)")
	flag.BoolVar(&verbose, "verbose", false, "hmm, tell me more....")
	flag.Parse()
	if "" == *targetIP {
		fmt.Fprintf(os.Stderr, "--target (IPv4) not provided\n")
		os.Exit(1)
	} else if !isValidIpV4(*targetIP) {
		fmt.Fprintf(os.Stderr, "target not valid IPv4 address\n")
		os.Exit(1)
	}
	fmt.Fprintf(os.Stderr, "will ping target:%s with connect timeout %d mS and completion timeout %d mS\n", *targetIP, *cConnect, *cComplete)

	tConnect, tDisrupt, tResume, tRedisrupt := core(*targetIP, *cConnect, *cComplete)
	fmt.Fprintf(os.Stderr, "tConnect\ttDisrupt\ttResume\tRedisrupt\n")
	fmt.Fprintf(os.Stdout, "%d\t%d\t%d\t%d\n", tConnect, tDisrupt, tResume, tRedisrupt)
}

func init() {
	if _, err := exec.LookPath("fping"); err != nil {
		fmt.Fprintf(os.Stderr, "fping binary not in path\n")
		os.Exit(1)
	}
}
