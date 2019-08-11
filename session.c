/* kakapo-session - a BGP traffic source and sink */

#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
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
#include <sys/uio.h>
#include <unistd.h>

#include "kakapo.h"
#include "libutil.h"
#include "stats.h"

#define BUFFSIZE 0x10000

#define NOTIFICATION_CEASE 6
#define NOTIFICATION_ADMIN_RESET 4

// wrapper for send which protects against multiple thread writes interleaving output
// the protection is 'soft' in that it introduces wait until queue empty semantics
// rather than an explicit semaphore
//int sendFlag = 0;
void _send(struct peer *p, const void *buf, size_t count) {
  txwait(p->sock);
  if (p->sendFlag != 0)
    die("send flag reentry fail");
  else
    p->sendFlag = 1;
  (0 < send(p->sock, buf, count, 0)) || die("send fail");
  txwait(p->sock);
  p->sendFlag = 0;
};

unsigned char notification[21] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0, 21, 3, 0, 0};
void send_notification(struct peer *p, unsigned char major, unsigned char minor) {
  notification[19] = major;
  notification[20] = minor;
  _send(p, notification, 21);
};
unsigned char keepalive[19] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0, 19, 4};
unsigned char marker[16] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};
const char *hexmarker = "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF";

char *bgpopen(int as, int holdtime, int routerid, char *hexoptions) {
  if (NULL == hexoptions) { // then we should build our own AS4 capability
                            // using the provided AS number
    // 020c = Optional Parameter = Capability, length 0x0c
    // 010400010001 = multiprotocol, AFI/SAFI 1/1
    // 4104xxxxxxxx AS4 capability, with ASn
    hexoptions = concat("020c", "010400010001", "4104", hex32(as), NULL);
    // hexoptions = concat("02064104", hex32(as), NULL);
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

void report(int expected, int got) {

  if (expected == got)
    fprintf(stderr, "session: OK, got %s\n", showtype(expected));
  else
    fprintf(stderr, "session: expected %s, got %s (%d)\n", showtype(expected), showtype(got), got);
}

void doopen(char *msg, int length) {
  unsigned char version = *(unsigned char *)msg;
  if (version != 4) {
    fprintf(stderr, "unexpected version in BGP Open %d\n", version);
  }
  uint16_t as = ntohs(*(uint16_t *)(msg + 1));
  uint16_t holdtime = ntohs(*(uint16_t *)(msg + 3));
  struct in_addr routerid = (struct in_addr){*(uint32_t *)(msg + 5)};
  unsigned char opl = *(unsigned char *)(msg + 9);
  unsigned char *hex = toHex(msg + 10, opl);
  fprintf(stderr, "BGP Open: as = %d, routerid = %s , holdtime = %d, opt params = %s\n", as, inet_ntoa(routerid), holdtime, hex);
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
  fprintf(stderr, "BGP Update(EOR) (End of RIB)\n");
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
    fprintf(stderr, "BGP Update: withdrawn count = %d, path attributes length = %d , NLRI count = %d\n", wc, tpal, uc);

  //**// if (ROLESENDER != p->role)
  //**//   updatelogrecord(p->slp, uc, wc, &(p->sb).rcvtimestamp);
};

void donotification(char *msg, int length) {
  unsigned char ec = *(unsigned char *)(msg + 0);
  unsigned char esc = *(unsigned char *)(msg + 1);
  fprintf(stderr, "BGP Notification: error code = %d, error subcode = %d\n", ec, esc);
};

int getBGPMessage(struct bgp_message *bm, struct sockbuf *sb) {
  char *header;

  bm->payload = 0;

  header = bufferedRead(sb, 19);
  if (header == (char *)-1) {
    bm->msgtype = BGPENDOFSTREAM;
  } else if (0 == header) {
    bm->msgtype = BGPTIMEOUT;
  } else if (!isMarker(header)) {
    // this is a differnt condition and should have a seprate value 'BGPUNSYNCHRONISED'
    die("Failed to find BGP marker in msg header from peer");
    bm->msgtype = BGPENDOFSTREAM;
  } else {
    bm->pl = (ntohs(*(uint16_t *)(header + 16))) - 19;
    bm->msgtype = *(char *)(header + 18);
    if (0 < bm->pl) {
      bm->payload = bufferedRead(sb, bm->pl);
      if (0 == bm->payload) {
        fprintf(stderr, "unexpected end of stream after header received\n");
        bm->msgtype = BGPENDOFSTREAM;
      }
    }
  }
}

struct bytestring build_update_block(int start, int length, uint32_t localip, uint32_t localpref) {

  assert(length <= TABLESIZE);
  int i;
  struct bytestring *vec = malloc(sizeof(struct bytestring) * length);
  int buflen = 0;

  for (i = start; i < start + length; i++) {
    int usn = i % TABLESIZE;
    int cyclenumber = 1 + i / TABLESIZE;
    struct bytestring b = update(nlris(SEEDPREFIX, SEEDPREFIXLEN, GROUPSIZE, usn), empty, iBGPpath(localip, (uint32_t[]){usn + SEEDPREFIX, cyclenumber, 0}));
    vec[i] = b;
    buflen += b.length;
  };
  char *data = malloc(buflen);
  char *offset = data;

  for (i = 0; i < length; i++) {
    offset = mempcpy(offset, vec[i].data, vec[i].length);
    free(vec[i].data);
  };
  return (struct bytestring){buflen, data};
};

void send_update_block(int offset, int length, struct peer *p) {

  // it appears the bgpd (openBGP) wants keepalives even if it is getting Updates!!!
  _send(p, keepalive, 19);
  struct bytestring updates = build_update_block(offset, length, p->localip, 100);
  _send(p, updates.data, updates.length);
  txwait(p->sock);
  free(updates.data);
};

void send_eor(struct peer *p) {

  struct bytestring b = update(empty, empty, empty);
  _send(p, b.data, b.length);
  txwait(p->sock);
  free(b.data);
};

// see documentation at https://docs.google.com/document/d/1CBWFJc1wbeyZ3Q4ilvn-NVAlWV3bzCm1PqP8B56_53Y
void *sendthread(void *_x) {
  struct peer *p = (struct peer *)p;

  uint32_t logseq;
  struct timespec tstart, tend;

  int sendupdates(int seq) {

    if (MAXBURSTCOUNT == 0)
      return -1;

    int bsn = seq % MAXBURSTCOUNT;
    int cyclenumber = seq / MAXBURSTCOUNT;
    if (((CYCLECOUNT > 0) && cyclenumber >= CYCLECOUNT) || (FASTCYCLELIMIT > 0 && logseq > CYCLECOUNT)) {
      fprintf(stderr, "%d: sendupdates: sending complete\n", p->tidx);
      return -1;
    };

    if (0 == seq) {
      gettime(&tstart);
      startlog(p->tidx, "N/A", &tstart);
    };

    if (cyclenumber >= FASTCYCLELIMIT || bsn == 0) {
      if (0 == cyclenumber && FASTCYCLELIMIT > 0)
        fprintf(stderr, "%d: FASTMODE START\n", p->tidx);
      logseq = senderwait();
      gettime(&tstart);
    };

    send_update_block(bsn, cyclenumber, p);

    if (cyclenumber >= FASTCYCLELIMIT || bsn == MAXBURSTCOUNT - 1) {
      gettime(&tend);
      sndlog(p->tidx, "N/A", logseq, &tstart, &tend);
      if (FASTCYCLELIMIT == cyclenumber && FASTCYCLELIMIT > 0 && bsn == 0)
        fprintf(stderr, "%d: FASTMODE END\n", p->tidx);
    };

    if (bsn == MAXBURSTCOUNT - 1)
      return CYCLEDELAY;
    else
      return 0; // ask to be restarted...
  };
  p->sndrunning = 1;
  timedloopms(SLEEP, sendupdates);

  senderwait();

  send_notification(p, NOTIFICATION_CEASE, NOTIFICATION_ADMIN_RESET);
  p->sndrunning = 0;
  tflag = 1;
  endlog(NULL); // note: endlog will probably never return!!!! ( calls exit() )
};

void init(struct peer *p) {
  int one = 1;
  setsockopt(p->sock, IPPROTO_TCP, TCP_NODELAY, (void *)&one, sizeof(one));
  one = 1;
  setsockopt(p->sock, IPPROTO_TCP, TCP_QUICKACK, (void *)&one, sizeof(one));
  bufferInit(&(p->sb), p->sock, BUFFSIZE, TIMEOUT);
};

void send_keepalive(struct peer *p) {
  _send(p, keepalive, 19);
};

void send_open(struct peer *p) {
  char *m = bgpopen(p->as, HOLDTIME, htonl(p->localip), NULL); // let the code build the optional parameter :: capability
  int ml = fromHex(m);
  _send(p, m, ml);
};

int expect_keepalive(struct peer *p) {

  struct bgp_message bm;

  getBGPMessage(&bm, &(p->sb));

  if (BGPKEEPALIVE == bm.msgtype) {
    return 0;
  } else {
    report(BGPKEEPALIVE, bm.msgtype);
    return -1;
  };
};

int expect_open(struct peer *p) {

  struct bgp_message bm;

  getBGPMessage(&bm, &(p->sb));

  if (BGPOPEN == bm.msgtype) {
    doopen(bm.payload, bm.pl);
    return 0;
  } else {
    report(BGPOPEN, bm.msgtype);
    return -1;
  };
};

void *session(void *x) {
  struct peer *p = (struct peer *)x;

  struct bgp_message bm;
  char *errormsg = "unspecified error";

  switch (p->role) {
  case ROLELISTENER:
    fprintf(stderr, "%d: session start - role=LISTENER\n", p->tidx);
    break;
  case ROLESENDER:
    fprintf(stderr, "%d: session start - role=SENDER\n", p->tidx);
    break;
  default:
    fprintf(stderr, "%d: session start - role=<unassigned>\n", p->tidx);
    goto exit;
  };

  init(p);
  send_open(p);
  if (0 != expect_open(p)) {
    goto exit;
  }

  pthread_exit(NULL);

  send_keepalive(p);
  if (0 != expect_keepalive(p))
    goto exit;

  pthread_t thrd;
  if (p->role == ROLESENDER) {
    p->sndrunning = 1;
    pthread_create(&thrd, NULL, sendthread, p);
  } else
    p->slp = initlogrecord(p->tidx, "N/A");

  while (0 == tflag) {
    if ((0 == p->sndrunning) && (p->role == ROLESENDER)) {
      errormsg = "sender exited unexpectedly";
      goto exit;
    };
    //    msgtype = getBGPMessage(&(p->sb)); // keepalive or updates from now on
    getBGPMessage(&bm, &(p->sb)); // keepalive or updates from now on
    switch (bm.msgtype) {
    case BGPTIMEOUT: // this is an idle recv timeout event
      break;
    case BGPUPDATE: // Update
      break;
    case BGPKEEPALIVE: // Keepalive
                       // trying to send while the send thread is also running is a BAD idea - because (using writev) the
                       // send here can interleave in the message flow!!!!!
      if (p->sndrunning == 0) {
        _send(p, keepalive, 19);
      };
      break;
    case BGPNOTIFICATION: // Notification
      fprintf(stderr, "%d: session: got Notification\n", p->tidx);
      errormsg = "got Notification";
      goto exit;
    default:
      if (bm.msgtype < 0) { // all non message events except recv timeout
        fprintf(stderr, "%d: session: end of stream\n", p->tidx);
        errormsg = "got end of stream";
      } else { // unexpected BGP message - unless BGP++ it must be an Open....
        // report(BGPUNKNOWN, bm.msgtype);
        errormsg = "got unexpected BGP message";
      }
      goto exit;
    }
  };
exit:
  closelogrecord(p->slp, p->tidx); // closelogrecord is safe in case that initlogrecord was not called...
  if (1 == p->sndrunning) {        // this guards against calling pthread_cancel on a thread which already exited
    pthread_cancel(thrd);
  };
  if (tflag) {
    send_notification(p, NOTIFICATION_CEASE, NOTIFICATION_ADMIN_RESET);
    errormsg = "shutdown requested";
    fprintf(stderr, "%d: shutdown requested\n", p->tidx);
  } else
    tflag = 1; // we still want the other side to close if we are exiting abnormally (maybe have another value of tflag to indicate an error exit?)
  close(p->sock);
  fprintf(stderr, "%d: session exit\n", p->tidx);
  // NB - endlog calls exit()!
  endlog(errormsg);
};

void *establish(void *x) {
  struct peer *p = (struct peer *)x;

  init(p);
  send_open(p);
  if (0 != expect_open(p)) {
    goto exit;
  }

  send_keepalive(p);
  if (0 != expect_keepalive(p))
    goto exit;

  // send_update_block(0, TABLESIZE, p);
  send_eor(p);

  pthread_exit(NULL);

exit:
  fprintf(stderr, "%d: abnormal session exit\n", p->tidx);
};
