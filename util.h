// util.h

#ifndef __UTIL_H
#define __UTIL_H
#include <sys/time.h>
#include <stdint.h>
#include <stdio.h>

typedef long long int inttime;
int die(char *mess);
char * fromHostAddress (uint32_t ip);
uint32_t toHostAddress (char * s );
int fromHex (char* s);
char * hex8(uint8_t n);
char * hex16(uint16_t n);
char * hex32(uint32_t n);
char * hex64(uint64_t n);
void printHex ( FILE * fd, char *buf, int l);
char *toHex (char *buf, int l);

inttime getinttime ();
inttime timeval_to_int (struct timeval *tval);

char* timeval_to_str (struct timeval *tval);

int timeval_subtract (struct timeval *result, struct timeval *x, struct timeval *y);
char * concat (const char *str,...);
#endif
