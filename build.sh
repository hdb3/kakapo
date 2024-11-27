#!/bin/bash -e
set +x
shopt -s expand_aliases
alias GCC="gcc -g -pthread -O3 -DBUILDDATE=\"\\\"$(date)\\\"\" -DVERSION=\"\\\"$(git describe)\\\"\""
BINDIR=`realpath $PWD/bin`
mkdir -p $BINDIR ||:
pushd core > /dev/null
#GCC -o nlri-unittest nlri-unittest.c libutil.c
set -x
GCC -o $BINDIR/logbuffer-test logbuffer-test.c libutil.c
GCC -o $BINDIR/kakapo main.c session.c stats.c libutil.c parsearg.c -lm
#GCC timedloop-unittest.c libutil.c -o timedloop-unittest
#GCC -o bytestring-unittest bytestring-unittest.c libutil.c
#GCC libutil.c update-unittest.c -o update-unittest
#GCC -I. test/hostaddress.c util.c -o test/hostaddress
set +x
echo "built version $(git describe)"

##
## the following macro expansions also works....
#alias GCC="gcc -g -pthread -O3 -D_GNU_SOURCE"
#VERSION="$(git describe)"
#DATE="$(date)"
#GCC -o kakapo main.c session.c stats.c libutil.c parsearg.c -DVERSION="\"$VERSION\"" -DBUILDDATE="\"$DATE\""

#DVERSION="-DVERSION=\"$(git describe)\""
#DDATE="-DBUILDDATE=\"$(date)\""
#GCC -o kakapo main.c session.c stats.c libutil.c parsearg.c "$DVERSION" "$DDATE"

popd >/dev/null

mkdir -p $HOME/.local/bin
cp -v $BINDIR/logbuffer-test $BINDIR/kakapo $HOME/.local/bin
