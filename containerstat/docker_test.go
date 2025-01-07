package main

import (
	// "gotest.tools/v3/assert"
	"fmt"
	"os"
	"testing"
)

func TestInitDockerMonitor(t *testing.T) {
	m := initDockerMonitor("bgpd")
	fmt.Fprintf(os.Stderr, "%#v\n", m)
}

func TestDockerMonitorStart(t *testing.T) {
	m := initDockerMonitor("bgpd")
	for m.state != StateRunning {
		m.doTickAction()
	}
}

func TestDockerMonitorAll(t *testing.T) {
	m := initDockerMonitor("bgpd")
	for m.state != StateRunning {
		m.doTickAction()
	}
	stats := m.read()
	fmt.Fprintf(os.Stderr, "%v\n", stats)
}
