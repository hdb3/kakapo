package main

/*
data source is the filesystem mounted as
/sys/fs/cgroup/system.slice/docker-${ID}.scope/memory.stat
where $ID is found only when the docker container is running by inspection:
$ docker container inspect $NAME --format '{{ .Id }}'
where sadly enclosing "double quotes" must be removed.
( $ id=`docker inspect --format='{{json .Id}}' bgpd| tr -d '"'` )

The first parameters known to be of interest are the fields
anon, inactive_anon, active_anon, where the first is the sum of the other two.
Each value is found on a line which starts with the name and is followed by space and the decimal encoded integer
*/

/*
Design notes

There is a state machine which refreshes on every tick action.
Initially, the wanted docker container may not have started,
and the FSM repeatedly polls a docker container status API with the wanted name.
Once the wanted container is running, it has a container ID which is the key to the proc filesystem for cgroups.
The status poll is actually a request for this ID.  When the ID is available, the FSM moves to 'running' mode,
and subsequent tick actions read the proc filesystem path calculated from the container ID,
based on the way docker daemon works under systemd.
The returned text slab contains the memory values needed, and a regex is used to extract them.
On the first occasion that reading the path fails, the sate goes to stopped,
and no more samples are read for the duration of the application

*/

import (
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strconv"
)

type dockerMonitor struct {
	state                   dockerState
	daemonName, containerId string
}

type containerStats struct {
	anon, inactive_anon, active_anon uint // TDOD - as this is measured in pages is it not at most uint32?
}

func (containerStats containerStats) show() string {
	return fmt.Sprintf("\t%d\t%d\t%d", containerStats.anon, containerStats.active_anon, containerStats.inactive_anon)
}

func (containerStats) hdr() string {
	return "\tanon\tactive_anon\tinactive_anon"
}

func initDockerMonitor(daemonName string) *dockerMonitor {
	monitor := new(dockerMonitor)
	monitor.daemonName = daemonName

	if path, err := exec.LookPath("docker"); err != nil {
		fmt.Fprintf(os.Stderr, "docker binary not in path\n")
		os.Exit(1)
		// in future return some error code
	} else {
		fmt.Printf("docker found at %s\n", path)
	}

	// check docker is actually available
	cmd := exec.Command("docker", "version", "--format", "{{ .Client.Version }}")
	if stdout, err := cmd.Output(); err != nil {
		fmt.Fprintf(os.Stderr, "docker version failed %s\n", err.Error())
		os.Exit(1)
		// in future return some error code
	} else {
		fmt.Fprintf(os.Stderr, "docker version %s\n", string(stdout))
	}

	return monitor
}

func dockerInspect(daemon, term string) (string, error) {
	cmd := exec.Command("docker", "container", "inspect", daemon, "--format", "{{ "+term+" }}")
	if stdout, err := cmd.Output(); err != nil {
		return "", err
	} else {
		// remove a single trailing newline from the stdout string (maybe could use strings.trim())
		return string(stdout[:len(stdout)-1]), nil
	}
}

func (monitor *dockerMonitor) getContainerId() bool {
	// could instead return the container Id and let the caller manage state
	// $ docker container inspect $NAME --format '{{ .Id }}'

	if id, err := dockerInspect(monitor.daemonName, ".Id"); err != nil {
		return false
	} else if running, err := dockerInspect(monitor.daemonName, ".State.Running"); err != nil {
		return false
	} else if running != "true" {
		return false
	} else {
		monitor.containerId = id
		fmt.Fprintf(os.Stderr, "container id for %s : %s\n", monitor.daemonName, monitor.containerId)
		return true
	}
}

func (monitor *dockerMonitor) read() *containerStats {
	contents := getContainerStat(monitor.containerId)
	// todo switch to have stringToContainerStats() return pointer, and so simplify
	if contents == "" {
		return nil
	} else {
		stats := stringToContainerStats(contents)
		return &stats
	}
}

func (monitor *dockerMonitor) doTickAction() (stats *containerStats) {
	switch monitor.state {
	case StateWait:
		// in initial state StateWait, check if the container has started by calling the 'inspect' method
		if monitor.getContainerId() {
			monitor.state = StateRunning
			fmt.Fprintf(os.Stderr, "container is started\n")
		}
	case StateRunning:
		if stats = monitor.read(); stats != nil {
			// no action needed, stats is the returned value in the default case
		} else {
			monitor.state = StateEnded
			fmt.Fprintf(os.Stderr, "container is stopped\n")
		}
	case StateEnded:
	}
	return
}

type dockerState int

const (
	StateWait dockerState = iota
	StateRunning
	StateEnded
)

var stateName = map[dockerState]string{
	StateWait:    "waiting",
	StateRunning: "running",
	StateEnded:   "ended",
}

func (ds dockerState) String() string {
	return stateName[ds]
}

func getContainerStat(containerId string) (infos string) {

	// path in some cases (modern ubuntu, docker installed from docker not distro),
	//  is /sys/fs/cgroup/system.slice/docker-${ID}.scope/memory.stat
	// see https://docs.docker.com/engine/containers/runmetrics/ in case of issues.

	path := "/sys/fs/cgroup/system.slice/docker-" + containerId + ".scope/memory.stat"

	if contents, err := os.ReadFile(path); err != nil {
		fmt.Fprintf(os.Stderr, "unexpected error reading %s\n", path)
		// should check that the problem is just that the container has exited
		// os.Exit(1)
		return ""
	} else {
		return string(contents)
	}
}

var regexAnon = regexp.MustCompile(`(?m)^(?:anon|inactive_anon|active_anon) ([0-9]{1,10})$`)

func stringToContainerStats(s string) (cs containerStats) {
	if matches := regexAnon.FindAllStringSubmatch(s, -1); matches == nil {
		fmt.Fprintf(os.Stderr, "regexAnon did not match\n")
	} else if len(matches) == 3 {
		for i := range matches {
			n, _ := strconv.Atoi(matches[i][1])
			switch strim(matches[i][0]) {
			case "anon":
				cs.anon = uint(n)
			case "inactive_anon":
				cs.inactive_anon = uint(n)
			case "active_anon":
				cs.active_anon = uint(n)
			default:
				fmt.Fprintf(os.Stderr, "something bad happened, unexpected key: %s\n", strim(matches[i][0]))
			}
		}

	} else {
		fmt.Fprintf(os.Stderr, "regexAnon mismatched: %v\n", matches)
	}
	return
}

// simple truncate up to first space
func strim(s string) (t string) {
	for _, c := range s {
		if c == ' ' {
			return
		} else {
			t = t + string(c)
		}
	}
	return
}
