#include <time.h>
#include <stdio.h>
//#include <pthread.h>
#include "timespec.h"

int f (int p) {
    fprintf(stderr,"f(%d)\n",p);
    return (p > 100);
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
       // while (delay > 0)
       while (timespec_gt(ts_target,now)) {
           struct timespec delay = timespec_sub(ts_target,now);
           nanosleep(&delay,&now);
           //nanosleep(timespec_sub(ts_target,now),&now);
           gettime(&now);
       };
   };
};

int main (int argc, char** argv) {

   struct timespec duration = {1,0};
   timedloop(duration,f);
};
