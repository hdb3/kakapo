

#include <stdio.h>
#include "util.h"

// unit test file for the fromHex function
// which is now in util.c

/*
int fromHex (char* s) {
    int i;
    for (i = 0; s[i+i] != 0 && s[i+i+1] != 0; i++) {
        sscanf(s+i+i, "%2hhx", s+i);
    }

    printf("%d bytes read\n",i);
    return i;
}
*/

int main(int argc, char **argv) {
    printf("converting %s\n",argv[1]);
    int n = fromHex (argv[1]);
    printHex(stdout,argv[1],n);
};
