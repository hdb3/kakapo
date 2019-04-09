#include <time.h>
#include <stdio.h>
#include "timespec.h"
#include "timedloop.h"

int f (int p) {
    fprintf(stderr,"f(%d)\n",p);
    return (p > 4);
};

int main (int argc, char** argv) {

   struct timespec duration = {1,0};

   timedloopms(50,f);
   timedloopms(500,f);
   timedloop(duration,f);
};
