#!/bin/bash -ve
gcc -pthread -O3 -D_GNU_SOURCE -o kakapo main.c session.c util.c sockbuf.c
