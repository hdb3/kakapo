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
	if verbose {
		fmt.Fprintf(os.Stderr, "command is: fping --quiet --retry %s --timeout %s %s\n", sRetry, sTimeout, target)
	}
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
		if s := stdout.String(); s != "" {
			fmt.Fprintf(os.Stderr, "stdout: '%s'\n", s)
		}
		if s := stderr.String(); s != "" {
			fmt.Fprintf(os.Stderr, "stderr: '%s'\n", s)
		}
		if cmd.ProcessState == nil {
			fmt.Fprintln(os.Stderr, "panic, cmd.ProcessState == nil")
			return false
		} else {
			fmt.Fprintf(os.Stderr, "exitCode: %d\n", cmd.ProcessState.ExitCode())
			return cmd.ProcessState.Success()
		}
	}
}

func pingTillReply(ip string, timeout int64) bool {
	return pingUntil(ip, true, timeout)
}

func pingTillNoReply(ip string, timeout int64) bool {
	return pingUntil(ip, false, timeout)
}

func pingUntil(ip string, p bool, timeout int64) bool {
	start := time.Now()
	for time.Since(start).Milliseconds() < timeout {
		if p == fping(ip, 1, tLoop) {
			return true
		} else {
			time.Sleep(tLoop * time.Millisecond)
		}
	}
	return false
}

func core(target string, cConnect, cComplete int64) (tConnect, tDisrupt, tResume, tRedisrupt int64) {
	start := time.Now()
	elapsed := func() int64 {
		return time.Since(start).Milliseconds()
	}

	if !pingTillReply(target, cConnect) {
		return
	} else {
		tConnect = elapsed()
	}

	if !pingTillNoReply(target, cComplete) {
		return
	} else {
		tDisrupt = elapsed()
	}

	if !pingTillReply(target, cComplete) {
		return
	} else {
		tResume = elapsed()
	}

	if !pingTillNoReply(target, cComplete) {
		return
	} else {
		tRedisrupt = elapsed()
	}
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
	fmt.Fprintf(os.Stderr, "breaktest - target:%s - connect timeout %dmS - completion timeout %dmS\n", *targetIP, *cConnect, *cComplete)

	tConnect, tDisrupt, tResume, tRedisrupt := core(*targetIP, *cConnect, *cComplete)
	fmt.Fprintf(os.Stderr, "tConnect\ttDisrupt\ttResume\ttRedisrupt\n")
	fmt.Fprintf(os.Stdout, "%d\t\t%d\t\t%d\t%d\n", tConnect, tDisrupt, tResume, tRedisrupt)
}

func init() {
	if _, err := exec.LookPath("fping"); err != nil {
		fmt.Fprintf(os.Stderr, "fping binary not in path\n")
		os.Exit(1)
	}
}
