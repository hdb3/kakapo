#include <time.h>
#include <stdio.h>
#include "timespec.h"
#include "timedloop.h"

int f (int p) {
    fprintf(stderr,"f(%d)\n",p);
    return (p > 9);
};

int main (int argc, char** argv) {

   struct timespec duration = {1,0};

   timedloop(duration,f);
};
