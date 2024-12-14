// util.h

#ifndef __UTIL_H
#define __UTIL_H
#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <time.h>

typedef long long int inttime;
int die(char *mess);
void gettime(struct timespec *ts);
char *showtime(struct timespec *ts);
char *shownow_prec(u_int8_t prec);
char *showtime_prec(struct timespec *ts, u_int8_t prec);
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
char *concat(const char *str, ...);
pthread_t _pthread_create(void *(*start_routine)(void *), void *arg);
void _pthread_join(pthread_t threadid);
#endif
