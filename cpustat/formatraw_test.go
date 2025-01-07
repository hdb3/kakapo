package main

import (
	// "gotest.tools/v3/assert"
	"fmt"
	"os"
	"testing"
	"time"
)

func TestTimeFormats(t *testing.T) {
	now := time.Now()
	fmt.Fprintf(os.Stderr, "TestTimeFormats\n")
	fmt.Fprintf(os.Stderr, "formatTimestamp: %s\n", formatTimestamp(now))
	fmt.Fprintf(os.Stderr, "humanFormatTimestamp: %s\n", humanFormatTimestamp(now))
	fmt.Fprintln(os.Stderr, "UnixMicro: ", now.UnixMicro())
	fmt.Fprintln(os.Stderr, "Unix: ", now.Unix())
	fmt.Fprintln(os.Stderr, "UnixMicro - Unix: ", now.UnixMicro()-now.Unix()*1000000)
	fmt.Fprintln(os.Stderr, "default format: ", now)
}
