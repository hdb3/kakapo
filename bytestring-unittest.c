#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <malloc.h>
#include "util.h"
#include "bytestring.h"

struct bytestring BS0  = { 0 , 0 };
struct bytestring BS1  = { 1 , "\x1" };
struct bytestring BS10  = { 10 , "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09" };
struct bytestring BS10a  = { 10 , "\x09\x08\x07\x06\x05\x04\x03\x02\x01\x00" };

int main(int argc, char** argv) {
    printf("BS0 %s\n",hexbytestring(BS0));
    printf("BS1 %s\n",hexbytestring(BS1));
    printf("BS10 %s\n",hexbytestring(BS10));
    struct bytestring cbs = concatbytestring(BS0,BS1,BS10,EOS);
    printf("concat result %s\n",hexbytestring(cbs));
    printf("concat BS10 ++ BS10a %s\n",hexbytestring(concatbytestring(BS10,BS10a,EOS)));
};
