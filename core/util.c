
// util.c

// ***** NOTE toHex and concat use malloc and may leak memory - use with caution!
// ***** NOTE also - non-thread-safe functions abound.

// ***** Possibly there exist portable standardised implementations of some functions here.
//       But I did not find an easy way to do that.

#include <math.h>

#include "libutil.h"

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

// show_ns_prec() - helper for showtime_prec()
char *show_ns_prec(uint64_t ns, u_int8_t prec) {
  // the ns value is some 64 bit type, and 'guaranteed' less than 10^9
  // if the calling function has concerns the guarantee is not sound,
  // it should normalise the struct timespec value before calling here.
  // This function is not thread safe.

  prec = (prec > 9) ? 9 : prec; // MAX function
  double fractional_seconds = (double)ns;
  double scale_factor = pow(10, 9 - prec);
  fractional_seconds /= scale_factor;
  fractional_seconds = round(fractional_seconds);
  int integral_fractional_seconds = (int)fractional_seconds;

  static char s[16];
  sprintf(s, "%0*d", (int)prec, integral_fractional_seconds);
  return s;
};

// in this version of showtime, the precision required for the fractional part is explicit
char *showtime_prec(struct timespec *ts, u_int8_t prec) {
  static char s[128];
  size_t len = strftime(s, 127, "%F %T", gmtime(&ts->tv_sec));
  if (prec > 0)
    sprintf(s + len, ".%s", show_ns_prec(ts->tv_nsec, prec));
  return s;
};

char *shownow_prec(u_int8_t prec) {
  struct timespec ts;
  gettime(&ts);
  return showtime_prec(&ts, prec);
};

// naive implementation, nano-second precision display
// char *showtime(struct timespec *ts) {
//   static char s[128];
//   size_t len = strftime(s, 127, "%F %T.", gmtime(&ts->tv_sec));
//   sprintf(s + len, "%09ld", ts->tv_nsec);
//   return s;
// };

// in these versions of showtime/shownow, the precision is arbitrarily set at 3 ~ millisecond precision
char *showtime(struct timespec *ts) {
  return showtime_prec(ts, 3);
};

char *shownow() {
  return shownow_prec(3);
};

double getdeltats(struct timespec ts) {
  struct timespec now;
  char *s;
  gettime(&now);
  return timespec_to_double(timespec_sub(now, ts));
};

char *showdeltats(struct timespec ts) {
  char *s;
  int tmp = asprintf(&s, "%01f", getdeltats(ts));
  return s;
};

char *fromHostAddress(uint32_t ip) {
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

pthread_t _pthread_create(void *(*start_routine)(void *), void *arg) {
  pthread_t threadid;
  pthread_create(&threadid, NULL, start_routine, arg);
  return threadid;
};

void _pthread_join(pthread_t threadid) {
  assert(0 == pthread_join(threadid, NULL));
};
