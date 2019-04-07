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
   printf("paOrigin ++ paLocalPref ++ paNextHop(%s) ++ paASPATH([65001,172,0]) %s\n", inet_ntoa(ip),
          hexbytestring(
              concatbytestring( pa2bytestring(paOrigin),
                                pa2bytestring(paLocalPref),
                                pa2bytestring(paNextHop(ip.s_addr)),
                                pa2bytestring(paASPATH(asnsa)),
                                EOS
                                )));
   printf("(quick) paOrigin ++ paLocalPref ++ paNextHop(%s) ++ paASPATH([65001,172,0]) %s\n", inet_ntoa(ip),
          hexbytestring(
              pas2bytestring( paOrigin,
                             paLocalPref,
                             paNextHop(ip.s_addr),
                             paASPATH(asnsa),
                             NULL
         )));
   printf("rewrite (2,deadbeef) paASPATH([65001,172,0]) %s\n", hexbytestring(pa2bytestring(rewriteASPATH(paASPATH(asnsa),0xdeadbeef,1))));
};
