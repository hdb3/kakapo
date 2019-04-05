
//util.c

#include <stdio.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/time.h>
#include <time.h>

#include "util.h"

int die(char *mess) { perror(mess); exit(1); }

// warning - this function replaces in situ!!!!!
// safe option w.r.t. length as the output is shorter than the input
int fromHex (char* s) {
    int i;
    for (i = 0; s[i+i] != 0 && s[i+i+1] != 0; i++) {
        sscanf(s+i+i, "%2hhx", s+i);
    }
    return i;
};

unsigned char *toHex (unsigned char *buf, unsigned int l) {

  unsigned char     hex_str[]= "0123456789abcdef";
  unsigned int      i;
  unsigned char *result;

  if (!(result = (unsigned char *)malloc(l * 2 + 1)))
    return (NULL);

  (result)[l * 2] = 0;

  if (!l)
    return (NULL);

  for (i = 0; i < l; i++)
    {
      (result)[i * 2 + 0] = hex_str[(buf[i] >> 4) & 0x0F];
      (result)[i * 2 + 1] = hex_str[(buf[i]     ) & 0x0F];
    }
  return (result);
}

void printHex ( FILE * fd, unsigned char *buf, unsigned int l) {
      unsigned char *hex = toHex (buf,l) ;
      fprintf(fd, "[%s]\n",hex);
      free(hex);
}


/* Subtract the ‘struct timeval’ values X and Y,
   storing the result in RESULT.
   Return 1 if the difference is negative, otherwise 0. */


long long int getinttime () {
   struct timeval tv;
   gettimeofday(&tv,NULL);
   return timeval_to_int(&tv);
};

long long int
timeval_to_int (struct timeval *tval)
{
  return (1000000*tval->tv_sec+tval->tv_usec);
}

char*
timeval_to_str (struct timeval *tval)
{
  char* tmp;
  assert (0 < asprintf(&tmp,"%ld.%06ld",tval->tv_sec,tval->tv_usec));
  return tmp;
}

int
timeval_subtract (struct timeval *result, struct timeval *x, struct timeval *y)
{
  /* Perform the carry for the later subtraction by updating y. */
  if (x->tv_usec < y->tv_usec) {
    int nsec = (y->tv_usec - x->tv_usec) / 1000000 + 1;
    y->tv_usec -= 1000000 * nsec;
    y->tv_sec += nsec;
  }
  if (x->tv_usec - y->tv_usec > 1000000) {
    int nsec = (x->tv_usec - y->tv_usec) / 1000000;
    y->tv_usec += 1000000 * nsec;
    y->tv_sec -= nsec;
  }

  /* Compute the time remaining to wait.
     tv_usec is certainly positive. */
  result->tv_sec = x->tv_sec - y->tv_sec;
  result->tv_usec = x->tv_usec - y->tv_usec;

  /* Return 1 if result is negative. */
  return x->tv_sec < y->tv_sec;
}

