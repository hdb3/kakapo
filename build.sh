#!/bin/bash -ve
gcc -O3 -D_GNU_SOURCE -o kakapo kakapo.c util.c sockbuf.c
