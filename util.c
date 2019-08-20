
// util.c

// ***** NOTE toHex and concat use malloc and may leak memory - use withh caution
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <netinet/in.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <time.h>
#include <pthread.h>

#include "timespec.h"
#include "util.h"

int die(char *mess) {
  if (0 != errno)
    perror(mess);
  else
    fprintf(stderr, "%s\n", mess);
  exit(1);
}

void gettime(struct timespec *ts) {
  int tmp = clock_gettime(CLOCK_REALTIME, ts);
};

char *showtime(struct timespec *ts) {
  static char s[128];
  ctime_r(&ts->tv_sec, s);
  // the following removes the terminating linefeed charcater!
  s[strlen(s) - 1] = 0;
  return s;
};

char *showdeltams(struct timespec ts) {
  struct timespec now;
  char *s;
  gettime(&now);
  int elapsed_ms = timespec_to_ms(timespec_sub(now, ts));
  int tmp = asprintf(&s, "%03d", elapsed_ms);
  return s;
};

char *shownow() {
  struct timespec ts;
  gettime(&ts);
  return showtime(&ts);
};

char *fromHostAddress(uint32_t ip) {
  // static char s [5];
  return inet_ntoa((struct in_addr){ip});
};

uint32_t toHostAddress(char *s) {
  struct in_addr addr;
  (0 != inet_aton(s, &addr)) ||
      (die("failed to convert string to host address"));
  return addr.s_addr;
};

// warning - this function replaces in situ!!!!!
// safe option w.r.t. length as the output is shorter than the input
int fromHex(char *s) {
  int i;
  for (i = 0; s[i + i] != 0 && s[i + i + 1] != 0; i++) {
    sscanf(s + i + i, "%2hhx", s + i);
  }
  return i;
};

char *hex8(uint8_t n) {
  char *s;
  int tmp = asprintf(&s, "%02hhX", n);
  return s;
};

char *hex16(uint16_t n) {
  char *s;
  int tmp = asprintf(&s, "%04hX", n);
  return s;
};

char *hex32(uint32_t n) {
  char *s;
  int tmp = asprintf(&s, "%08X", n);
  return s;
};

char *hex64(uint64_t n) {
  char *s;
  int tmp = asprintf(&s, "%016lX", n);
  return s;
};

char *toHex(char *buf, int l) {

  char hex_str[] = "0123456789abcdef";
  int i;
  char *result;

  // ** DANGER - unreturned memory allocation!!!!
  if (!(result = (char *)malloc(l * 2 + 1)))
    return (NULL);

  (result)[l * 2] = 0;

  if (!l)
    return (NULL);

  for (i = 0; i < l; i++) {
    (result)[i * 2 + 0] = hex_str[(buf[i] >> 4) & 0x0F];
    (result)[i * 2 + 1] = hex_str[(buf[i]) & 0x0F];
  }
  return (result);
}

void printHex(FILE *fd, char *buf, int l) {
  char *hex = toHex(buf, l);
  fprintf(fd, "[%s]\n", hex);
  free(hex);
}

/* Subtract the ‘struct timeval’ values X and Y,
   storing the result in RESULT.
   Return 1 if the difference is negative, otherwise 0. */

inttime getinttime() {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return timeval_to_int(&tv);
};

inttime timeval_to_int(struct timeval *tval) {
  return (1000000 * tval->tv_sec + tval->tv_usec);
}

char *timeval_to_str(struct timeval *tval) {
  char *tmp;
  assert(0 < asprintf(&tmp, "%ld.%06ld", tval->tv_sec, tval->tv_usec));
  return tmp;
}

int timeval_subtract(struct timeval *result, struct timeval *x,
                     struct timeval *y) {
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

// the following function copied directly from the glibc manual

char *concat(const char *str, ...) {
  va_list ap;
  size_t allocated = 100;
  // ** DANGER - unreturned memory allocation!!!!
  char *result = (char *)malloc(allocated);

  if (result != NULL) {
    char *newp;
    char *wp;
    const char *s;

    va_start(ap, str);

    wp = result;
    for (s = str; s != NULL; s = va_arg(ap, const char *)) {
      size_t len = strlen(s);

      /* Resize the allocated memory if necessary.  */
      if (wp + len + 1 > result + allocated) {
        allocated = (allocated + len) * 2;
        newp = (char *)realloc(result, allocated);
        if (newp == NULL) {
          free(result);
          return NULL;
        }
        wp = newp + (wp - result);
        result = newp;
      }

      wp = mempcpy(wp, s, len);
    }

    /* Terminate the result string.  */
    *wp++ = '\0';

    /* Resize memory to the optimal size.  */
    newp = realloc(result, wp - result);
    if (newp != NULL)
      result = newp;

    va_end(ap);
  }

  return result;
}

pthread_t _pthread_create( void *(*start_routine) (void *), void *arg) {
  pthread_t threadid;
  pthread_create(&threadid, NULL, start_routine, arg);
  return threadid;
};

void _pthread_join(pthread_t threadid) {
  assert (0 == pthread_join(threadid, NULL));
};
