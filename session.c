
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
#include <sys/uio.h>

#include "kakapo.h"
#include "libutil.h"
#include "session.h"
#include "stats.h"

#define BUFFSIZE 0x10000

#define BGPENDOFSTREAM (-1)
#define BGPTIMEOUT 0
#define BGPOPEN 1
#define BGPUPDATE 2
#define BGPNOTIFICATION 3
#define BGPKEEPALIVE 4
#define BGPUNKNOWN 5

#define NOTIFICATION_CEASE 6
#define NOTIFICATION_ADMIN_RESET 4

void *session(void *x) {
  // from here on down all variables are function, and thus thread, local.
  struct sessiondata *sd = (struct sessiondata *)x;

  uint32_t localip, peerip;
  slp_t slp = NULL;

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
    ((0 == getsockname(sd->sock, &sockaddr, &socklen) && (socklen == SOCKADDRSZ)) || die("Failed to find local address"));
    localip = sockaddr.sin_addr.s_addr;

    memset(&sockaddr, 0, SOCKADDRSZ);
    socklen = SOCKADDRSZ;
    ((0 == getpeername(sd->sock, &sockaddr, &socklen) && (socklen == SOCKADDRSZ)) || die("Failed to find peer address"));
    peerip = sockaddr.sin_addr.s_addr;

    fprintf(stderr, "%s: connection info: %s/", tid, fromHostAddress(localip));
    fprintf(stderr, "%s\n", fromHostAddress(peerip));
  };

  unsigned char notification[21] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0, 21, 3, 0 ,0 };
  void send_notification(int sock, unsigned char major, unsigned char minor) {
    notification[19]=major;
    notification[20]=minor;
    (0 < send(sock, notification, 21, 0)) || die("Failed to send notification to peer");
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

  char *showtype(char msgtype) {
    switch (msgtype) {
    case BGPENDOFSTREAM:
      return "ENDOFSTREAM";
      break;
    case BGPTIMEOUT:
      return "TIMEOUT";
      break;
    case BGPOPEN:
      return "OPEN";
      break;
    case BGPUPDATE:
      return "UPDATE";
      break;
    case BGPNOTIFICATION:
      return "NOTIFICATION";
      break;
    case BGPKEEPALIVE:
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
    fprintf(stderr, "%s: BGP Open: as = %d, routerid = %s , holdtime = %d, opt params = %s\n", tid, as, inet_ntoa(routerid), holdtime, hex);
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
        fprintf(stderr, "**** %d %d %d %d %s \n", nlri[offset], offset, count, length, hex);
        assert(0);
      }
      if ((1 == VERBOSE) && (offset < length))
        printPrefix(nlri + offset + 1, nlri[offset]);
    }

    // debug only
    if (offset != length) {
      unsigned char *hex = toHex(nlri, length);
      fprintf(stderr, "**** %d %d %d %d %s \n", nlri[offset], offset, count, length, hex);
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
      fprintf(stderr, "%s: BGP Update: withdrawn count = %d, path attributes length = %d , NLRI count = %d\n", tid, wc, tpal, uc);

    if (ROLESENDER != sd->role)
      updatelogrecord(slp, uc, wc, &sb.rcvtimestamp);
  };

  void donotification(char *msg, int length) {
    unsigned char ec = *(unsigned char *)(msg + 0);
    unsigned char esc = *(unsigned char *)(msg + 1);
    fprintf(stderr, "%s: BGP Notification: error code = %d, error subcode = %d\n", tid, ec, esc);
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
      return BGPENDOFSTREAM;
    } else if (0 == header) {
      return BGPTIMEOUT;
    } else if (!isMarker(header)) {
      die("Failed to find BGP marker in msg header from peer");
      return BGPENDOFSTREAM;
    } else {
      pl = (ntohs(*(uint16_t *)(header + 16))) - 19;
      msgtype = *(char *)(header + 18);
      if (0 < pl) {
        payload = bufferedRead(sb, pl);
        if (0 == payload) {
          fprintf(stderr, "%s: unexpected end of stream after header received\n", tid);
          return BGPENDOFSTREAM;
        }
      } else
        payload = 0;
    }
    if (1 == VERBOSE) {
      unsigned char *hex = toHex(payload, pl);
      fprintf(stderr, "%s: BGP msg type %s length %d received [%s]\n", tid, showtype(msgtype), pl, hex);
      free(hex);
    }

    switch (msgtype) {
    case BGPOPEN:
      doopen(payload, pl);
      break;
    case BGPUPDATE:
      if (pl == 4)
        doeor(payload, pl);
      else
        doupdate(payload, pl);
      break;
    case BGPNOTIFICATION:
      donotification(payload, pl);
      break;
    case BGPKEEPALIVE: // keepalive, no analysis required
      break;
    };
    return msgtype;
  }

  void report(int expected, int got) {

    if (1 == VERBOSE) {
      if (expected == got) {
        fprintf(stderr, "%s: session: OK, got %s\n", tid, showtype(expected));
      } else {
        fprintf(stderr, "%s: session: expected %s, got %s (%d)\n", tid, showtype(expected), showtype(got), got);
      }
    } else {
      if (expected != got)
        fprintf(stderr, "%s: session: expected %s, got %s (%d)\n", tid, showtype(expected), showtype(got), got);
    }
  }

  int sndrunning = 0;

// see documentation at https://docs.google.com/document/d/1CBWFJc1wbeyZ3Q4ilvn-NVAlWV3bzCm1PqP8B56_53Y/edit?usp=sharing
  void *sendthread(void *_x) {

    uint32_t logseq;
    struct timespec tstart, tend;

    int sendupdates(int seq) {

      if (MAXBURSTCOUNT == 0)
        return -1;

      int bsn = seq % MAXBURSTCOUNT;
      int cyclenumber = seq / MAXBURSTCOUNT;
      if (((CYCLECOUNT > 0) && cyclenumber >= CYCLECOUNT) || (FASTCYCLELIMIT > 0 && logseq > CYCLECOUNT)) {
        fprintf(stderr, "%s: sendupdates: sending complete\n", tid);
        return -1;
      };

      if (0 == seq) {
        gettime(&tstart);
        startlog(sd->tidx, tid, &tstart);
      };

      if (cyclenumber >= FASTCYCLELIMIT || bsn == 0) {
        if (0==cyclenumber && FASTCYCLELIMIT > 0)
          fprintf(stderr, "%s: FASTMODE START\n", tid);
        logseq = senderwait();
        gettime(&tstart);
      };

      int i, usn;
      int iovecbuflen = 0;
      struct iovec *vec = malloc(sizeof(struct iovec) * BLOCKSIZE);

      // the loopcount runs from 0 to BLOCKSIZE -1, + a fixed offset (bsn * BLOCKSIZE)
      // an array to hold the entire output is exactly BLOCKSIZE in size
      // for (usn = bsn * BLOCKSIZE; usn < (bsn + 1) * BLOCKSIZE; usn++) {
      for (i = 0; i < BLOCKSIZE; i++) {
        usn = i + bsn * BLOCKSIZE;
        struct bytestring b = update(nlris(SEEDPREFIX, SEEDPREFIXLEN, GROUPSIZE, usn), empty, iBGPpath(localip, (uint32_t[]){usn + SEEDPREFIX, cyclenumber + 1, 0}));
        vec[i].iov_base = b.data; 
        vec[i].iov_len = b.length; 
        iovecbuflen += b.length;
        //if (0 == sendbs(sock, b))
          //return -1;
        // eBGPpath(localip, (uint32_t[]){usn + SEEDPREFIX, cyclenumber + 1, sd->as, 0})));
      };
      ssize_t wcount = writev(sock, vec , BLOCKSIZE);
      int shortwrite = (wcount < iovecbuflen);
      while (wcount < iovecbuflen) {
         ssize_t deltawcount = pwritev(sock, vec , BLOCKSIZE, wcount);
         wcount += deltawcount;
      };
      free(vec);

      if (shortwrite)
        fprintf(stderr, "%s: sendupdates: multiple writes required\n", tid);


      if (cyclenumber >= FASTCYCLELIMIT || bsn == MAXBURSTCOUNT-1) {
        gettime(&tend);
        sndlog(sd->tidx, tid, logseq, &tstart, &tend);
        if (FASTCYCLELIMIT==cyclenumber && FASTCYCLELIMIT > 0 && bsn ==0 )
          fprintf(stderr, "%s: FASTMODE END\n", tid);
      };

      if (bsn == MAXBURSTCOUNT - 1)
        return CYCLEDELAY;
      else
        return 0; // ask to be restarted...
    };
    sndrunning = 1;
    timedloopms(SLEEP, sendupdates);

    senderwait();

    // potentially could send NOTIFICATION here.....
    send_notification(sock,NOTIFICATION_CEASE,NOTIFICATION_ADMIN_RESET);
    sndrunning = 0;
    tflag=1;
    endlog(NULL); // note: endlog will probably never return!!!! ( calls exit() )
  };

  long int threadmain() {

    char *errormsg = "unspecified error";

    switch (sd->role) {
    case ROLELISTENER:
      fprintf(stderr, "%s: session start - role=LISTENER\n", tid);
      break;
    case ROLESENDER:
      fprintf(stderr, "%s: session start - role=SENDER\n", tid);
      break;
    default:
      fprintf(stderr, "%s: session start - role=<unassigned>\n", tid);
      goto exit;
    };

    getsockaddresses();

    int one = 1;
    setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (void *)&one, sizeof(one));
    one = 1;
    setsockopt(sock, IPPROTO_TCP, TCP_QUICKACK, (void *)&one, sizeof(one));
    bufferInit(&sb, sock, BUFFSIZE, TIMEOUT);

    char *m = bgpopen(sd->as, HOLDTIME, htonl(localip), NULL); // let the code build the optional parameter :: capability
    int ml = fromHex(m);
    FLAGS(sock, __FILE__, __LINE__);
    (0 < send(sock, m, ml, 0)) || die("Failed to send synthetic open to peer");
    FLAGS(sock, __FILE__, __LINE__);

    do
      msgtype = getBGPMessage(&sb); // this is expected to be an Open
    while (msgtype == BGPTIMEOUT);

    report(BGPOPEN, msgtype);
    if (BGPOPEN != msgtype)
      goto exit;

    FLAGS(sock, __FILE__, __LINE__);
    (0 < send(sock, keepalive, 19, 0)) || die("Failed to send keepalive to peer");
    FLAGS(sock, __FILE__, __LINE__);

    do
      msgtype = getBGPMessage(&sb); // this is expected to be a Keepalive
    while (msgtype == BGPTIMEOUT);

    report(BGPKEEPALIVE, msgtype);
    if (BGPKEEPALIVE != msgtype)
      goto exit;

    pthread_t thrd;
    if (sd->role == ROLESENDER) {
      sndrunning = 1;
      pthread_create(&thrd, NULL, sendthread, NULL);
    } else
      slp = initlogrecord(sd->tidx, tid);


    while (0==tflag) {
      if ((0 == sndrunning) && (sd->role == ROLESENDER)) {
        errormsg = "sender exited unexpectedly";
        goto exit;
      };
      msgtype = getBGPMessage(&sb); // keepalive or updates from now on
      switch (msgtype) {
      case BGPTIMEOUT: // this is an idle recv timeout event
        break;
      case BGPUPDATE: // Update
        break;
      case BGPKEEPALIVE: // Keepalive
        FLAGS(sock, __FILE__, __LINE__);
        (0 < send(sock, keepalive, 19, 0)) || die("Failed to send keepalive to peer");
        FLAGS(sock, __FILE__, __LINE__);
        break;
      case BGPNOTIFICATION: // Notification
        fprintf(stderr, "%s: session: got Notification\n", tid);
        errormsg = "got Notification";
        goto exit;
      default:
        if (msgtype < 0) { // all non message events except recv timeout
          fprintf(stderr, "%s: session: end of stream\n", tid);
          errormsg = "got end of stream";
        } else { // unexpected BGP message - unless BGP++ it must be an Open....
          report(BGPUNKNOWN, msgtype);
          errormsg = "got unexpected BGP message";
        }
        goto exit;
      }
    };
  exit:
    closelogrecord(slp, sd->tidx); // closelogrecord is safe in case that initlogrecord was not called...
    if (1 == sndrunning) {         // this guards against calling pthread_cancel on a thread which already exited
      pthread_cancel(thrd);
    };
    if (tflag) {
      send_notification(sock,NOTIFICATION_CEASE,NOTIFICATION_ADMIN_RESET);
      errormsg = "shutdown requested";
      fprintf(stderr, "%s: shutdown requested\n", tid);
    } else
      tflag=1; // we still want the other side to close if we are exiting abnormally (maybe have another value of tflag to indicate an error exit?)
    close(sock);
    fprintf(stderr, "%s: session exit\n", tid);
    free(sd);
    // NB - endlog calls exit()!
    endlog(errormsg);
  } // end of threadmain

  // effective start of 'main, i.e. function 'session'
  // all code and variables are defined within 'session' function to ensure that
  // the variables are thread local, and to allow inner functions access to
  // those local variables

  return (int *)threadmain();
}
