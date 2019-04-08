#include <stdio.h>
#include <stdint.h>
#include <arpa/inet.h>
#include "libutil.h"

int main (int argc, char** argv) {

   struct in_addr ip;
   inet_aton("192.168.0.1",&ip);
   uint32_t asns [] = {65001,0};
   uint32_t asnsa [] = {65001,172,0};

   // //printf("%s\n", hexbytestring(pa2bytestring(paASPATH(asns))));
   // //printf("%s\n", hexbytestring(pa2bytestring(paASPATH(asnsa))));
   // //printf("%s\n", hexbytestring(pa2bytestring(paASPATH((uint32_t []) {65001,0} ))));
   // //printf("%s\n", hexbytestring(pa2bytestring(paASPATH((uint32_t []) {1,2,3,0} ))));
   // //struct bytestring pth = iBGPpath(toHostAddress("192.168.1.1"), (uint32_t []) {1,2,3,0} );
   // //printf("%s\n", hexbytestring(iBGPpath(toHostAddress("192.168.1.1"), (uint32_t []) {1,2,3,0} )));
   // //printf("%s\n", hexbytestring(pth));
   struct bytestring nlri =  nlris(toHostAddress("10.0.0.0"),30,4);
   // //printf("nlri%s\n", hexbytestring(nlri));
   printf("update(empty,empty,empty)%s\n", hexbytestring(update(empty,empty,empty)));
   printf("update(nrli,empty,empty)%s\n", hexbytestring(update(nlri,empty,empty)));


   //struct bytestring nlri = nlris(ip.s_addr,24,4);
   // //struct bytestring path = iBGPpath (toHostAddress("192.168.1.1"), asns);
   struct bytestring path = iBGPpath (toHostAddress("192.168.1.1"), (uint32_t []) {1,2,3,0});
   struct bytestring withdrawn =  empty;
   struct bytestring msg = update (nlri,withdrawn,path);

   printf("path: %s\n", hexbytestring(path));
   printf("withdrawn: %s\n", hexbytestring(withdrawn));
   printf("nlri: %s\n", hexbytestring(nlri));
   printf("iBGPupdate: %s\n", hexbytestring(msg));
   printf("iBGPupdate: %s\n", hexbytestring(update ( nlris(toHostAddress("10.0.0.0"),30,4),
                                                     empty,
                                                     iBGPpath (toHostAddress("192.168.1.1"), (uint32_t []) {1,2,3,0})
                                           )));
   printf("iBGPwithdraw: %s\n", hexbytestring(update ( empty,
                                                       nlris(toHostAddress("10.0.0.0"),30,4),
                                                       empty
                                           )));

   return 0;

   struct bytestring attributes = pas2bytestring( paOrigin,
                                                  paLocalPref,
                                                  paNextHop(ip.s_addr),
                                                  paASPATH(asnsa),
                                                  NULL );

   printf("update (nlri 192.168.0.0/24 , 4) (empty) paOrigin ++ paLocalPref ++ paNextHop(%s) ++ paASPATH([65001,172,0]) %s\n", inet_ntoa(ip) ,
           hexbytestring(update (nlri,withdrawn,attributes)));


   printf("nlri 192.168.0.0/24 , 4 %s\n", hexbytestring(nlris(ip.s_addr,24,4)));

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
