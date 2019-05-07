
/* kakapo-session - a BGP traffic source and sink */

#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <linux/sockios.h>
#include <net/if.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/sendfile.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <unistd.h>

#include "kakapo.h"
#include "libutil.h"
#include "session.h"
#include "stats.h"

#define MAXPENDING 5 // Max connection requests
#define BUFFSIZE 0x10000

void *session(void *x) {
  // from here on down all variables are function, and thus thread, local.
  struct sessiondata *sd = (struct sessiondata *)x;
  slp_t slp = NULL;

  uint32_t localip, peerip;

  int i, msgtype;
  struct sockbuf sb;
  struct timeval t_active, t_idle;
  int active = 0;
  int sock = sd->sock;
  char *tid;
  int tmp = asprintf(&tid, "%d-%d: ", pid, sd->tidx);

  void getsockaddresses() {
    struct sockaddr_in sockaddr;
    int socklen;

    memset(&sockaddr, 0, SOCKADDRSZ);
    socklen = SOCKADDRSZ;
    ((0 == getsockname(sd->sock, &sockaddr, &socklen) &&
      (socklen == SOCKADDRSZ)) ||
     die("Failed to find local address"));
    localip = sockaddr.sin_addr.s_addr;

    memset(&sockaddr, 0, SOCKADDRSZ);
    socklen = SOCKADDRSZ;
    ((0 == getpeername(sd->sock, &sockaddr, &socklen) &&
      (socklen == SOCKADDRSZ)) ||
     die("Failed to find peer address"));
    peerip = sockaddr.sin_addr.s_addr;

    fprintf(stderr, "%s: connection info: %s/", tid, fromHostAddress(localip));
    fprintf(stderr, "%s\n", fromHostAddress(peerip));
  };

  unsigned char keepalive[19] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0, 19, 4};
  unsigned char marker[16] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};
  const char *hexmarker = "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF";

  char *bgpopen(int as, int holdtime, int routerid, char *hexoptions) {
    if (NULL == hexoptions) { // then we should build our own AS4 capability
                              // using the provided AS number
      hexoptions = concat("02064104", hex32(as), NULL);
    };

    char *hexmessage =
        concat(hex8(4), hex16(as), hex16(holdtime), hex32(routerid),
               hex8(strlen(hexoptions) / 2), hexoptions, NULL);
    int messagelength = strlen(hexmessage) / 2 + 19;
    char *ret =
        concat(hexmarker, hex16(messagelength), hex8(1), hexmessage, NULL);
    return ret;
  };

  int isMarker(const unsigned char *buf) {
    return (0 == memcmp(buf, marker, 16));
  }

  char *showtype(unsigned char msgtype) {
    switch (msgtype) {
    case 1:
      return "OPEN";
      break;
    case 2:
      return "UPDATE";
      break;
    case 3:
      return "NOTIFICATION";
      break;
    case 4:
      return "KEEPALIVE";
      break;
    default:
      return "UNKNOWN";
    }
  }

  void doopen(char *msg, int length) {
    unsigned char version = *(unsigned char *)msg;
    if (version != 4) {
      fprintf(stderr, "%s: unexpected version in BGP Open %d\n", tid, version);
    }
    uint16_t as = ntohs(*(uint16_t *)(msg + 1));
    uint16_t holdtime = ntohs(*(uint16_t *)(msg + 3));
    struct in_addr routerid = (struct in_addr){*(uint32_t *)(msg + 5)};
    unsigned char opl = *(unsigned char *)(msg + 9);
    unsigned char *hex = toHex(msg + 10, opl);
    fprintf(stderr,
            "%s: BGP Open: as =  %d, routerid = %s , holdtime = %d, opt params "
            "= %s\n",
            tid, as, inet_ntoa(routerid), holdtime, hex);
    free(hex);
  };

  void printPrefix(char *pfx, int length) {
    uint32_t addr = ntohl(*((uint32_t *)pfx));
    uint32_t mask = (0xffffffff >> (32 - length)) << (32 - length);
    uint32_t maskedaddr = htonl(addr & mask);
    struct in_addr inaddr = (struct in_addr){maskedaddr};
    fprintf(stderr, "%s/%d\n", inet_ntoa(inaddr), length);
  };

  // simpleParseNLRI
  int spnlri(char *nlri, int length) {
    int count = 0;
    int offset = 0;
    for (; offset < length;) {
      count++;
      if (nlri[offset] == 0)
        offset += 1;
      else if (nlri[offset] < 9)
        offset += 2;
      else if (nlri[offset] < 17)
        offset += 3;
      else if (nlri[offset] < 25)
        offset += 4;
      else if (nlri[offset] < 33)
        offset += 5;
      else {
        unsigned char *hex = toHex(nlri, length);
        fprintf(stderr, "**** %d %d %d %d %s \n", nlri[offset], offset, count,
                length, hex);
        assert(0);
      }
      if ((1 == VERBOSE) && (offset < length))
        printPrefix(nlri + offset + 1, nlri[offset]);
    }

    // debug only
    if (offset != length) {
      unsigned char *hex = toHex(nlri, length);
      fprintf(stderr, "**** %d %d %d %d %s \n", nlri[offset], offset, count,
              length, hex);
    }
    assert(offset == length);
    return count;
  }

  void doeor(char *msg, int length) {
    uint16_t wrl = ntohs(*(uint16_t *)msg);
    assert(0 == wrl);
    uint16_t tpal = ntohs(*(uint16_t *)(msg + 2));
    assert(0 == tpal);
    fprintf(stderr, "%s: BGP Update(EOR) (End of RIB)\n", tid);
  };

  void doupdate(char *msg, int length) {
    uint16_t wrl = ntohs(*(uint16_t *)msg);
    assert(wrl < length - 1);
    char *withdrawn = msg + 2;
    uint16_t tpal = ntohs(*(uint16_t *)(msg + wrl + 2));
    char *pa = msg + wrl + 4;
    assert(wrl + tpal < length - 3);
    char *nlri = msg + wrl + tpal + 4;
    uint16_t nlril = length - wrl - tpal - 4;
    int wc, uc;
    if (wrl > 0)
      wc = spnlri(withdrawn, wrl);
    else
      wc = 0;
    if (nlril > 0)
      uc = spnlri(nlri, nlril);
    else
      uc = 0;

    if (1 == VERBOSE)
      fprintf(stderr,
              "%s: BGP Update: withdrawn count =  %d, path attributes length = "
              "%d , NLRI count = %d\n",
              tid, wc, tpal, uc);

    if (ROLESENDER != sd->role)
      updatelogrecord(slp, uc, wc, &sb.rcvtimestamp);
  };

  void donotification(char *msg, int length) {
    unsigned char ec = *(unsigned char *)(msg + 0);
    unsigned char esc = *(unsigned char *)(msg + 1);
    fprintf(stderr,
            "%s: BGP Notification: error code =  %d, error subcode = %d\n", tid,
            ec, esc);
  };

  int getBGPMessage(struct sockbuf * sb) {
    char *header;
    char *payload;
    int received;
    uint16_t pl;
    unsigned char msgtype;

    header = bufferedRead(sb, 19);
    if (header == (char *)-1) {
      fprintf(stderr, "%s: end of stream\n", tid);
      return -1;
    } else if (0 == header) {
      // zero is simply a timeout: -1 is eof
      return 0;
    } else if (!isMarker(header)) {
      die("Failed to find BGP marker in msg header from peer");
      return -1;
    } else {
      pl = (ntohs(*(uint16_t *)(header + 16))) - 19;
      msgtype = *(unsigned char *)(header + 18);
      if (0 < pl) {
        payload = bufferedRead(sb, pl);
        if (0 == payload) {
          fprintf(stderr,
                  "%s: unexpected end of stream after header received\n", tid);
          return 0;
        }
      } else
        payload = 0;
    }
    if (1 == VERBOSE) {
      unsigned char *hex = toHex(payload, pl);
      fprintf(stderr, "%s: BGP msg type %s length %d received [%s]\n", tid,
              showtype(msgtype), pl, hex);
      free(hex);
    }

    switch (msgtype) {
    case 1:
      doopen(payload, pl);
      break;
    case 2:
      if (pl == 4)
        doeor(payload, pl);
      else
        doupdate(payload, pl);
      break;
    case 3:
      donotification(payload, pl);
      break;
    case 4: // keepalive, no analysis required
      break;
    };
    return msgtype;
  }

  void report(int expected, int got) {

    if (1 == VERBOSE) {
      if (expected == got) {
        fprintf(stderr, "%s: session: OK, got %s\n", tid, showtype(expected));
      } else {
        fprintf(stderr, "%s: session: expected %s, got %s (%d)\n", tid,
                showtype(expected), showtype(got), got);
      }
    } else {
      if (expected != got)
        fprintf(stderr, "%s: session: expected %s, got %s (%d)\n", tid,
                showtype(expected), showtype(got), got);
    }
  }

  int sndrunning = 0;
  void *sendthread(void *_x) {

    int sendupdates(int seq) {

      struct timespec tstart, tend;

      if ((MAXBURSTCOUNT == 0) || (sd->role == ROLELISTENER))
        return -1;

      int cyclenumber = seq / MAXBURSTCOUNT;
      if ((CYCLECOUNT > 0) && cyclenumber >= CYCLECOUNT) {
        fprintf(stderr, "%s: sendupdates: sending complete\n", tid);
        return -1;
      };

      gettime(&tstart);
      if ((0 == seq) && (sd->role == ROLESENDER))
        startlog(sd->tidx, tid, &tstart);

      uint32_t logseq;
      if (sd->role == ROLESENDER)
        logseq = senderwait();

      int bsn = seq % MAXBURSTCOUNT;

      gettime(&tstart);
      int usn;
      for (usn = bsn * BLOCKSIZE; usn < (bsn + 1) * BLOCKSIZE; usn++) {
        sendbs(sock,
               update(nlris(SEEDPREFIX, SEEDPREFIXLEN, GROUPSIZE, usn), empty,
                      iBGPpath(localip, (uint32_t[]){usn + SEEDPREFIX,
                                                     cyclenumber + 1, 0})));
        // eBGPpath(localip, (uint32_t[]){usn + SEEDPREFIX, cyclenumber + 1,
        // sd->as, 0})));
      };
      gettime(&tend);

      if (sd->role == ROLESENDER)
        sndlog(sd->tidx, tid, logseq, &tstart, &tend);

      if (bsn == MAXBURSTCOUNT - 1)
        return CYCLEDELAY;
      else
        return 0; // ask to be restarted...
    };
    sndrunning = 1;
    timedloopms(SLEEP, sendupdates);
    if (sd->role == ROLESENDER) {
      senderwait();
      endlog();
    };
    sndrunning = 0;
  };

  long int threadmain() {

    switch (sd->role) {
    case ROLELISTENER:
      fprintf(stderr, "%s: session start - role=LISTENER\n", tid);
      break;
    case ROLESENDER:
      fprintf(stderr, "%s: session start - role=SENDER\n", tid);
      break;
    default:
      fprintf(stderr, "%s: session start - role=<unassigned>\n", tid);
    };

    getsockaddresses();

    int one = one;
    setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (void *)&one, sizeof(one));
    setsockopt(sock, IPPROTO_TCP, TCP_QUICKACK, (void *)&one, sizeof(one));
    bufferInit(&sb, sock, BUFFSIZE, TIMEOUT);

    // char * m =
    // bgpopen(65001,180,htonl(inet_addr("192.168.122.123")),"020641040000fde8");
    char *m = bgpopen(
        sd->as, HOLDTIME, htonl(localip),
        NULL); // let the code build the optional parameter :: capability
    int ml = fromHex(m);
    flags(sock,__FILE__,__LINE__);
    (0 < send(sock, m, ml, 0)) || die("Failed to send synthetic open to peer");
    flags(sock,__FILE__,__LINE__);

    do
      msgtype = getBGPMessage(&sb); // this is expected to be an Open
    while (msgtype == 0);

    report(1, msgtype);
    if (1 != msgtype)
      goto exit;

    flags(sock,__FILE__,__LINE__);
    (0 < send(sock, keepalive, 19, 0)) ||
        die("Failed to send keepalive to peer");
    flags(sock,__FILE__,__LINE__);

    do
      msgtype = getBGPMessage(&sb); // this is expected to be a Keepalive
    while (msgtype == 0);

    report(4, msgtype);
    if (4 != msgtype)
      goto exit;

    pthread_t thrd;
    pthread_create(&thrd, NULL, sendthread, NULL);

    if (sd->role != ROLESENDER)
      slp = initlogrecord(sd->tidx, tid);

    while (1) {
      msgtype = getBGPMessage(&sb); // keepalive or updates from now on
      switch (msgtype) {
      case 2: // Update
        break;
      case 4: // Keepalive
        flags(sock,__FILE__,__LINE__);
        (0 < send(sock, keepalive, 19, 0)) ||
            die("Failed to send keepalive to peer");
        flags(sock,__FILE__,__LINE__);
        break;
      case 0: // this is an idle recv timeout event
        break;
      case 3: // Notification
        fprintf(stderr, "%s: session: got Notification\n", tid);
        goto exit;
      default:
        if (msgtype < 0) { // all non message events except recv timeout
          fprintf(stderr, "%s: session: end of stream\n", tid);
        } else { // unexpected BGP message - unless BGP++ it must be an Open....
          report(2, msgtype);
        }
        goto exit;
      }
    };
  exit:
    closelogrecord(slp, sd->tidx); // closelogrecord is safe in case that initlogrecord was not called...
    if (1 == sndrunning)           // this guards against calling pthread_cancel on a thread which already exited
      pthread_cancel(thrd);
    close(sock);
    fprintf(stderr, "%s: session exit\n", tid);
    free(sd);
  } // end of threadmain

  // effective start of 'main, i.e. function 'session'
  // all code and variables are defined within 'session' function to ensure that
  // the variables are thread local, and to allow inner functions access to
  // those local variables

  return (int *)threadmain();
}
