// util.h

#ifndef __UTIL_H
#define __UTIL_H
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>

typedef long long int inttime;
int die(char *mess);
void gettime(struct timespec *ts);
char *showtime(struct timespec *ts);
double getdeltats(struct timespec ts);
char *showdeltats(struct timespec ts);
char *shownow();
char *fromHostAddress(uint32_t ip);
uint32_t toHostAddress(char *s);
int fromHex(char *s);
char *hex8(uint8_t n);
char *hex16(uint16_t n);
char *hex32(uint32_t n);
char *hex64(uint64_t n);
void printHex(FILE *fd, char *buf, int l);
char *toHex(char *buf, int l);

inttime getinttime();
inttime timeval_to_int(struct timeval *tval);

char *timeval_to_str(struct timeval *tval);

int timeval_subtract(struct timeval *result, struct timeval *x,
                     struct timeval *y);
#define TSZERO ((struct timespec){0, 0})
char *concat(const char *str, ...);
pthread_t _pthread_create(void *(*start_routine)(void *), void *arg);
void _pthread_join(pthread_t threadid);
#endif
