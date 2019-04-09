#!/bin/bash -ve
gcc -pthread -O3 -D_GNU_SOURCE -o kakapo main.c session.c libutil.c
#gcc -pthread -O3 -D_GNU_SOURCE -o kakapo main.c session.c util.c sockbuf.c timedloop.c timespec.c
#gcc -pthread -O3 -DVERBOSE=0 -DTIMEOUT=1 -D_GNU_SOURCE -o kakapo.loop main.c session.c libutil.c
gcc -D_GNU_SOURCE -pthread -O3 timedloop-unittest.c libutil.c -o timedloop-unittest
gcc -D_GNU_SOURCE -O3 -o bytestring-unittest bytestring-unittest.c libutil.c
gcc -D_GNU_SOURCE -O3 libutil.c update-unittest.c -o update-unittest
#gcc -D_GNU_SOURCE -O3 -o update-unittest update-unittest.c pathattributes.c bytestring.c util.c nlri.c update.c
#gcc -pthread -O3 -DVERBOSE=1 -DTIMEOUT=1 -D_GNU_SOURCE -o kakapo.loop main.c session.c util.c sockbuf.c
gcc -D_GNU_SOURCE -I. test/hostaddress.c util.c -o test/hostaddress
