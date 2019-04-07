#include <stdio.h>
#include <stdint.h>
#include <arpa/inet.h>
#include "bytestring.h"
#include "update.h"

int main (int argc, char** argv) {

struct in_addr ip;
inet_aton("192.168.0.1",&ip);
uint32_t asns [] = {65001,0};
uint32_t asnsa [] = {65001,172,0};
//uint32_t asns [] = {0xffffffff,0};
//uint32_t asnsa [] = {0x01020304,0xffffffff,0};

   printf("paOrigin %s\n", hexbytestring(pa2bytestring(paOrigin)));
   printf("paLocalPref %s\n", hexbytestring(pa2bytestring(paLocalPref)));
   printf("paMED %s\n", hexbytestring(pa2bytestring(paMED)));
   printf("paNextHop(%s) %s\n", inet_ntoa(ip), hexbytestring(pa2bytestring(paNextHop(ip.s_addr))));
   printf("paASPATH([65001,0]) %s\n", hexbytestring(pa2bytestring(paASPATH(asns))));
   printf("paASPATH([65001,172,0]) %s\n", hexbytestring(pa2bytestring(paASPATH(asnsa))));
};
