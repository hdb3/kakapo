#!/bin/bash -ve
gcc -g -pthread -O3 -DTIMEOUT=1 -D_GNU_SOURCE -o kakapo main.c session.c util.c sockbuf.c
gcc -g -pthread -O3 -DMYAS=65007 -DSLEEP=5000 -DVERBOSE=0 -DTIMEOUT=1 -D_GNU_SOURCE -o kakapo.loop main.c session.c util.c sockbuf.c
#gcc -pthread -O3 -DSLEEP=5000 -DVERBOSE=1 -DTIMEOUT=1 -D_GNU_SOURCE -o kakapo.loop main.c session.c util.c sockbuf.c
