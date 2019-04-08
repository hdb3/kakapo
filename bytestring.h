#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <malloc.h>
#include "util.h"

#ifndef _BYTESTRING_H
#define _BYTESTRING_H

struct bytestring { uint16_t length ; char* data; };
struct bytestring empty;
struct bytestring EOS;
char * hexbytestring ( struct bytestring bs );
struct bytestring concatbytestring(struct bytestring bs0 , ...);
void sendbs(int sock, struct bytestring msg );
#endif // _BYTESTRING_H
