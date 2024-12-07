#!/bin/bash -e

SOURCES="main.c session.c stats.c libutil.c parsearg.c"
FLAGS="-g -pthread -O3"
LFLAGS="-lm -luuid"

DEFINES=("-D_GNU_SOURCE")
DEFINES+=("-DBUILDDATE=\"$(date)\"")
DEFINES+=("-DVERSION=\"$(git describe)\"")
DEFINES+=("-DBRANCH=\"$(git branch --show-current)\"")

rm -f ./kakapo
gcc $FLAGS "${DEFINES[@]}" -o kakapo $SOURCES $LFLAGS
./kakapo --version
