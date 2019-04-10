#!/bin/bash -ve
shopt -s expand_aliases
alias GCC='gcc -pthread -O3 -D_GNU_SOURCE'
GCC -o kakapo main.c session.c stats.c libutil.c
GCC -g -o kakapo.debug main.c session.c stats.c libutil.c
GCC timedloop-unittest.c libutil.c -o timedloop-unittest
GCC -o bytestring-unittest bytestring-unittest.c libutil.c
GCC libutil.c update-unittest.c -o update-unittest
GCC -I. test/hostaddress.c util.c -o test/hostaddress
