#!/bin/bash -e

SOURCES="main.c session.c stats.c libutil.c parsearg.c"
FLAGS="-g -pthread -O3"
# FLAGS="-g -pthread -O3g -ggdb3 -fno-omit-frame-pointer"
LFLAGS="-lm -luuid"

DEFINES=("-D_GNU_SOURCE")
DEFINES+=("-DBUILDDATE=\"$(date)\"")
DEFINES+=("-DVERSION=\"$(git describe)\"")
DEFINES+=("-DBRANCH=\"$(git branch --show-current)\"")

rm -f ./kakapo
gcc $FLAGS "${DEFINES[@]}" -o kakapo $SOURCES $LFLAGS
./kakapo --version

# # uncomment the next lines to enable build of some unit test functions...
# # Build any temporary unit test.
# gcc -g "${DEFINES[@]}" -o unit unit.c libutil.c $LFLAGS
