#!/bin/bash -ex
gcc -g -O3 -pthread -D_GNU_SOURCE main.c util.c -o relay
gcc -g -O3 -pthread -D_GNU_SOURCE relay2.c util.c -o relay2
