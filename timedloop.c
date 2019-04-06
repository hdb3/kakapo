#include <time.h>
#include <stdio.h>
#include "timespec.h"
#include "timedloop.h"

void timedloopms (int duration, int (action) (int)) {
    timedloop ( (struct timespec) { duration / 1000 , 1000000000 * ( duration % 1000)}, action);
};

void timedloopsec (int duration, int (action) (int)) {
    timedloop ( (struct timespec) { duration , 0}, action);
};

void timedloop (struct timespec duration, int (action) (int)) {

   struct timespec delay, ts_target, ts_entry, ts_exit, now;
   void gettime (struct timespec *ts) { int tmp = clock_gettime(CLOCK_REALTIME,ts); };
   int i = 0;
   int result;

   gettime(&ts_target);
   while (1) {
       gettime(&ts_entry);
       result = action(i);
       gettime(&ts_exit);
       if ( result ) break;
       ts_target = timespec_add (ts_target,duration);
       i++;
       gettime(&now);
       while (timespec_gt(ts_target,now)) {
           struct timespec delay = timespec_sub(ts_target,now);
           nanosleep(&delay,&now);
           gettime(&now);
       };
   };
};
