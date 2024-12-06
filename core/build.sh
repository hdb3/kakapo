#!/bin/bash -xe
shopt -s expand_aliases
alias GCC="gcc -g -pthread -O3 -D_GNU_SOURCE -DBUILDDATE=\"\\\"$(date)\\\"\" -DVERSION=\"\\\"$(git describe)\\\"\""
GCC -o kakapo main.c session.c stats.c libutil.c parsearg.c -lm
set +v
echo "built version $(git describe)"
