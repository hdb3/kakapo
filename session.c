/* kakapo-session - a BGP traffic source and sink */

#define _GNU_SOURCE
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

// __send: variant od _send which does not use txwait
void __send(struct peer *p, const void *buf, size_t count) {
  size_t sent, total_sent;
  // printf("******send: %ld\n", count);
  if (p->sendFlag != 0)
    die("send flag reentry fail");
  else
    p->sendFlag = 1;
  total_sent = 0;
  do {
    sent = send(p->sock, buf + total_sent, count - total_sent, 0);
    if (sent == -1)
      die("send fail");
    total_sent += sent;
    if (total_sent < count)
      printf("******total_sent<count: %ld %ld\n", total_sent, count);
  } while (total_sent < count);
  // printf("******done: %ld\n", count);
  p->sendFlag = 0;
};

void _send(struct peer *p, const void *buf, size_t count) {
  int sent;
  txwait(p->sock);
  __send(p, buf, count);
  txwait(p->sock);
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

void doeor(char *msg) {
  uint16_t wrl = ntohs(*(uint16_t *)msg);
  assert(0 == wrl);
  uint16_t tpal = ntohs(*(uint16_t *)(msg + 2));
  assert(0 == tpal);
  fprintf(stderr, "BGP Update(EOR) (End of RIB)\n");
};

struct bytestring get_nlri(struct bytestring update) {
  uint16_t wrl = ntohs(*(uint16_t *)(update.data));
  assert(wrl < update.length - 1);
  uint16_t tpal = ntohs(*(uint16_t *)(update.data + wrl + 2));
  assert(wrl + tpal < update.length - 3);
  char *nlri = update.data + wrl + tpal + 4;
  uint16_t nlril = update.length - wrl - tpal - 4;
  if (0 == nlril)
    return (struct bytestring){0, 0};
  else
    return (struct bytestring){nlril, nlri};
};

struct bytestring get_withdrawn(struct bytestring update) {
  uint16_t wrl = ntohs(*(uint16_t *)(update.data));
  assert(wrl < update.length - 1);
  if (0 == wrl)
    return (struct bytestring){0, 0};
  else
    return (struct bytestring){wrl, update.data + 2};
};

struct bytestring get_path(struct bytestring update) {
  uint16_t wrl = ntohs(*(uint16_t *)(update.data));
  assert(wrl < update.length - 1);
  uint16_t tpal = ntohs(*(uint16_t *)(update.data + wrl + 2));
  assert(wrl + tpal < update.length - 3);
  char *path = update.data + wrl + 4;
  if (0 == tpal)
    return (struct bytestring){0, 0};
  else
    return (struct bytestring){tpal, path};
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

static uint64_t usn = 0;
#define TEN7 10000000
struct bytestring build_update_block(int peer_index, int length, uint32_t localip, uint32_t localpref) {

  assert(length <= TABLESIZE);
  uint64_t i;
  struct bytestring *vec = malloc(sizeof(struct bytestring) * length);
  uint64_t buflen = 0;

  for (i = 0; i < length; i++) {
    struct bytestring b = update(nlris(SEEDPREFIX, SEEDPREFIXLEN, GROUPSIZE, i),
                                 empty,
                                 iBGPpath(localip,
                                          localpref,
                                          (uint32_t[]){i + TEN7, peer_index, TEN7 + usn / TEN7, TEN7 + usn % TEN7, 0}));
    vec[i] = b;
    buflen += b.length;
    usn++;
  };
  char *data = malloc(buflen);
  char *offset = data;

  for (i = 0; i < length; i++) {
    offset = mempcpy(offset, vec[i].data, vec[i].length);
    free(vec[i].data);
  };
  free(vec);
  return (struct bytestring){buflen, data};
};

void send_update_block(int offset, int length, struct peer *p) {

  // it appears the bgpd (openBGP) wants keepalives even if it is getting Updates!!!
  // _send(p, keepalive, 19);
  uint32_t localpref = ((0 == length) ? ((0 == usn) ? 100 : 99) : 101 + usn / TABLESIZE);
  struct bytestring updates = build_update_block(p->tidx, ((0 == length) ? TABLESIZE : length), p->localip, localpref);
  _send(p, updates.data, updates.length);
  free(updates.data);
};

void send_next_update(struct peer *p) {
  uint32_t localpref = 101 + usn / TABLESIZE;
  struct bytestring b = update(nlris(SEEDPREFIX, SEEDPREFIXLEN, GROUPSIZE, usn % TABLESIZE),
                               empty,
                               iBGPpath(p->localip,
                                        localpref,
                                        (uint32_t[]){TEN7 + usn % TABLESIZE, p->tidx, TEN7 + usn / TEN7, TEN7 + usn % TEN7, 0}));
  usn++;
  // printf("send_next_update for peer %d %s %d\n",p->tidx,fromHostAddress(p->localip),p->sock);
  __send(p, b.data, b.length);
  free(b.data);
};

void send_single_update(struct peer *p, uint32_t ip, uint8_t length) {
  struct bytestring b = update(nlris(ip, length, 1, 0), empty, iBGPpath(p->localip, 100, (uint32_t[]){p->tidx, TEN7 + usn / TEN7, TEN7 + usn % TEN7, 0}));
  usn++;
  _send(p, b.data, b.length);
  free(b.data);
};

void send_single_withdraw(struct peer *p, uint32_t ip, uint8_t length) {
  struct bytestring b = update(empty, nlris(ip, length, 1, 0), empty);
  _send(p, b.data, b.length);
  free(b.data);
};

void send_eor(struct peer *p) {

  struct bytestring b = update(empty, empty, empty);
  _send(p, b.data, b.length);
  // txwait(p->sock);
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

typedef int(pf_t)(void *, struct bgp_message *);
typedef void *(thread_t)(void *);

struct crf_state {
  struct timespec start;
  struct timespec end;
  int status;
  int (*pf)(void *, struct bgp_message *);
  struct peer *p;
  void *pf_state;
};

struct crf_state *crf(struct crf_state *crfs, int (*pf)(void *, struct bgp_message *), void *pf_state, struct peer *p) {

  struct bgp_message bm;
  crfs->status = 0;

  gettime(&crfs->start);
  while (crfs->status == 0) {
    getBGPMessage(&bm, &(p->sb));
    if (BGPKEEPALIVE == bm.msgtype)
      continue;
    if (BGPUPDATE == bm.msgtype) {
      // ignore EOR
      if (bm.pl == 4)
        doeor(bm.payload);
      else
        crfs->status = pf(pf_state, &bm);
      continue;
    } else {
      fprintf(stderr, "crf: exception exit\n");
      report(BGPUPDATE, bm.msgtype);
      crfs->status = -1;
    }
  };
  gettime(&crfs->end);
  return crfs;
};

struct crf_state *crfw(struct crf_state *crfs) {
  crf(crfs, crfs->pf, crfs->pf_state, crfs->p);
};

pthread_t crfp(int (*pf)(void *, struct bgp_message *), void *pf_state, struct peer *p) {
  pthread_t threadid;
  struct crf_state *crfs = calloc(1, sizeof(struct crf_state));
  crfs->p = p;
  crfs->pf = pf;
  crfs->pf_state = pf_state;
  pthread_create(&threadid, NULL, (thread_t *)crfw, crfs);
  return threadid;
};

struct crf_state crfjoin(pthread_t threadid) {
  struct crf_state *crfsp;
  void **retval = (void **)&crfsp;
  pthread_join(threadid, retval);
  return *crfsp;
};

int pf_withdrawn(int *counter, struct bgp_message *bm) {
  struct bytestring msg = (struct bytestring){bm->pl, bm->payload};
  struct bytestring withdrawn = get_withdrawn(msg);
  int withdrawn_count = nlri_count(withdrawn);
  (*counter) -= withdrawn_count;
  return (*counter > 0 ? 0 : 1);
};

void crf_withdrawn_count(int counter, struct crf_state *crfs, struct peer *p) {
  int counter_end = counter;
  crf(crfs, (pf_t *)pf_withdrawn, &counter_end, p);
  if (1 != crfs->status)
    printf("** WARNING - crf_withdrawn_count - counter start: %d counter end: %d\n", counter, counter_end);
};

int pf_update(struct prefix *pfx, struct bgp_message *bm) {
  struct bytestring msg = (struct bytestring){bm->pl, bm->payload};
  struct bytestring nlri = get_nlri(msg);
  return (nlri_member(nlri, *pfx));
};

struct crf_state *crf_update(struct prefix *pfx, struct crf_state *crfs, struct peer *p) {
  return crf(crfs, (pf_t *)pf_update, pfx, p);
};

int pf_count(int *counter, struct bgp_message *bm) {
  (*counter)--;
  return (*counter > 0 ? 0 : 1);
};

pthread_t crf_count(int *counter, struct crf_state *crfs, struct peer *p) {
  return crfp((pf_t *)pf_count, counter, p);
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
  struct crf_state crfs;

  init(p);
  send_open(p);
  if (0 != expect_open(p)) {
    goto exit;
  }

  send_keepalive(p);
  if (0 != expect_keepalive(p))
    goto exit;

  send_eor(p);

  pthread_exit(NULL);

exit:
  fprintf(stderr, "establish: abnormal exit\n");
};

void *crf_canary_thread(struct peer *p) {
  struct timespec ts;
  struct crf_state crfs;
  struct prefix pfx = (struct prefix){CANARYSEED + __bswap_32((p + 1)->tidx), 32};
  fprintf(stderr, "crf_test listener : %s\n", fromHostAddress(p->localip));
  gettime(&ts);
  crf_update(&pfx, &crfs, p);
  fprintf(stderr, "crf_test listener return status=%d elapsed time %s\n", crfs.status, showdeltams(ts));
};

void *crf_canary_test(struct peer *p) {
  struct timespec ts;
  pthread_t crf_threadid;

  pthread_create(&crf_threadid, NULL, (thread_t *)crf_canary_thread, p);

  fprintf(stderr, "crf_canary_test sender   : %s\n", fromHostAddress((p + 1)->localip));
  gettime(&ts);
  send_update_block(0, TABLESIZE, p + 1);
  fprintf(stderr, "crf_canary_test block transmit elapsed time %s\n", showdeltams(ts));
  sleep(2);
  send_single_update((p + 1), CANARYSEED + __bswap_32((p + 1)->tidx), 32);
  fprintf(stderr, "crf_canary_test canary transmit elapsed time %s\n", showdeltams(ts));
  pthread_join(crf_threadid, NULL);
  fprintf(stderr, "crf_canary_test total elapsed time %s\n", showdeltams(ts));
};

void *crf_thread(struct peer *p) {
  struct timespec ts;
  struct crf_state crfs;
  int count = TABLESIZE;
  fprintf(stderr, "crf_test listener : %s\n", fromHostAddress(p->localip));
  gettime(&ts);
  crf_count(&count, &crfs, p);
  fprintf(stderr, "crf_test listener return status=%d elapsed time %s\n", crfs.status, showdeltams(ts));
};

void *crf_test(struct peer *p) {
  struct timespec ts;
  pthread_t crf_threadid;

  pthread_create(&crf_threadid, NULL, (thread_t *)crf_thread, p);

  fprintf(stderr, "crf_test sender   : %s\n", fromHostAddress((p + 1)->localip));
  gettime(&ts);
  send_update_block(0, TABLESIZE, p + 1);
  fprintf(stderr, "crf_test transmit elapsed time %s\n", showdeltams(ts));
  pthread_join(crf_threadid, NULL);
  fprintf(stderr, "crf_test total elapsed time %s\n", showdeltams(ts));
};

/*
*/

double single_peer_burst_test(struct peer *p, int count) {
  struct crf_state crfs;
  struct timespec ts_start, ts_end;
  double elapsed;
  int n = count;

  gettime(&ts_start);

  pthread_t threadid = crf_count(&n, &crfs, p);
  send_update_block(0, count, p + 1);
  gettime(&ts_end);
  elapsed = timespec_to_double(timespec_sub(ts_end, ts_start));
  fprintf(stderr, "single_peer_burst_test(%d) transmit elapsed time %f\n", count, elapsed);
  crfs = crfjoin(threadid);
  elapsed = timespec_to_double(timespec_sub(crfs.end, crfs.start));
  fprintf(stderr, "single_peer_burst_test receive elapsed time %f status %d\n", elapsed, crfs.status);
  return elapsed;
};

void *strict_canary(struct peer *listener, struct peer *p) {
  struct timespec ts;
  struct crf_state crfs;
  struct prefix pfx = (struct prefix){CANARYSEED + __bswap_32(p->tidx), 32};

  gettime(&ts);
  send_single_update(p, CANARYSEED + __bswap_32(p->tidx), 32);
  crf_update(&pfx, &crfs, listener);

  send_single_withdraw(p, CANARYSEED + __bswap_32(p->tidx), 32);
  crf_withdrawn_count(1, &crfs, listener);
};

void *rx_thread(struct peer *p) {
  struct crf_state crfs;
  struct prefix pfx = (struct prefix){CANARYSEED + __bswap_32((p + 1)->tidx), 32};
  crf_update(&pfx, &crfs, p);
};

pthread_t rx_start(struct peer *p) {
  pthread_t threadid;

  pthread_create(&threadid, NULL, (thread_t *)rx_thread, p);
  return threadid;
};

void rx_end(struct peer *p, pthread_t threadid) {
  send_single_update((p + 1), CANARYSEED + __bswap_32((p + 1)->tidx), 32);
  pthread_join(threadid, NULL);
};

void *conditioning_single_peer(struct peer *target, struct peer *listen) {
  struct timespec ts;

  pthread_t threadid = rx_start(listen);
  gettime(&ts);
  fprintf(stderr, "conditioning : %s\n", fromHostAddress(target->localip));
  send_update_block(0, TABLESIZE, target);
  rx_end(listen, threadid);
  fprintf(stderr, "conditioning complete: elapsed time %s\n", showdeltams(ts));
};

void *strict_canary_all(struct peer *p) {
  struct timespec ts;
  struct peer *listener = p;
  gettime(&ts);
  while ((++p)->sock != 0) {
    strict_canary(listener, p);
  };
  fprintf(stderr, "strict_canary_all complete: elapsed time %s\n", showdeltams(ts));
};

struct burst_receive {
  struct peer *p;
  int count;
};

void *burst_receive_thread(struct burst_receive *br) {
  struct timespec ts;
  struct crf_state crfs;
  gettime(&ts);
  crf_count(&br->count, &crfs, br->p);
  //fprintf(stderr, "burst_receive listener return status=%d elapsed time %s\n", crfs.status, showdeltams(ts));
};

double multi_peer_burst_test(struct peer *p, int count) {
  struct timespec ts_start, ts_end;
  double elapsed;
  pthread_t threadid;
  struct burst_receive br = {p, count};
  struct peer *sender;
  int sent = 0;
  //fprintf(stderr, "multi_peer_burst_test(%d) start\n", count);
  pthread_create(&threadid, NULL, (thread_t *)burst_receive_thread, &br);
  gettime(&ts_start);
  do {
    sender = p;
    while ((++sender)->sock != 0) {
      send_next_update(sender);
      sent++;
      if (sent == count)
        break;
    };
  } while (sent < count);
  //fprintf(stderr, "multi_peer_burst_test(%d) transmit complete: elapsed time %s\n", count, showdeltams(ts));
  pthread_join(threadid, NULL);
  //strict_canary_all(p);
  gettime(&ts_end);
  elapsed = timespec_to_double(timespec_sub(ts_end, ts_start));
  fprintf(stderr, "multi_peer_burst_test(%d) complete: elapsed time %f\n", count, elapsed);
  return elapsed;
};

void *conditioning(struct peer *p) {
  struct timespec ts;
  struct peer *listener = p;
  gettime(&ts);
  fprintf(stderr, "conditioning start\n");
  while ((++p)->sock != 0) {
    conditioning_single_peer(p, listener);
  };
  fprintf(stderr, "conditioning complete: elapsed time %s\n", showdeltams(ts));
};

void *notify_all(struct peer *p) {
  struct timespec ts;

  gettime(&ts);
  while ((p)->sock != 0) {
    // fprintf(stderr, "Notification from : %s\n", fromHostAddress(p->localip));
    send_notification(p, NOTIFICATION_CEASE, NOTIFICATION_ADMIN_RESET);
    p++;
  };
  fprintf(stderr, "Notification complete: elapsed time %s\n", showdeltams(ts));
};

//
// Coniuous mode test support
//

int bgp_receive(struct peer *p) {
  struct bgp_message bm;

  while (1) {
    getBGPMessage(&bm, &(p->sb));
    if (BGPKEEPALIVE == bm.msgtype)
      continue;
    if (BGPUPDATE == bm.msgtype) {
      // ignore EOR
      if (bm.pl == 4)
        continue;
      else
        return 1;
    } else {
      report(BGPUPDATE, bm.msgtype);
      return 0;
    }
  }
};

struct logger_local {
  struct log_record first_lr;
  struct log_record last_lr;
};

int logger(struct logbuffer *lb, struct logger_local *llp) {
  struct log_record *lrp;
  lrp = logbuffer_read(lb);
  while (NULL != lrp) {
    if (0 == lrp->ts.tv_sec)
      return 1;
    else {
      // printf("index=%d ts=%f\n", lrp->index, timespec_to_double(lrp->ts));
      if (0 == lrp->index) { // first entry
        llp->first_lr = *lrp;
        llp->last_lr = *lrp;
      } else { // 2nd or later entry, rate calculation is possible
        struct timespec aggregate_duration = timespec_sub(lrp->ts, llp->first_lr.ts);
        int aggregate_count = lrp->index * lb->block_size;
        struct timespec cycle_duration = timespec_sub(lrp->ts, llp->last_lr.ts);
        int cycle_count = (lrp->index - llp->last_lr.index) * lb->block_size;
        printf("aggregate rate = %d (%d/%f)\n", (int)(aggregate_count / timespec_to_double(aggregate_duration)), aggregate_count, timespec_to_double(aggregate_duration));
        printf("current rate = %d (%d/%f)\n", (int)(cycle_count / timespec_to_double(cycle_duration)), cycle_count, timespec_to_double(cycle_duration));
      }
    };
    llp->last_lr = *lrp;
    lrp = logbuffer_read(lb);
  };

  return 0;
};

void *logging_thread(struct logbuffer *lb) {

  struct timespec ts_delay, ts_target, ts_entry, ts_exit, ts_now;
  struct logger_local *llp = malloc(sizeof(struct logger_local));

  gettime(&ts_target);

  while (1) {
    gettime(&ts_now);
    ts_delay = timespec_sub(ts_target, ts_now);
    while (ts_delay.tv_sec > 0 || (ts_delay.tv_sec == 0 && ts_delay.tv_nsec > 0))
      if (0 == nanosleep(&ts_delay, &ts_delay))
        break;
    if (logger(lb, llp))
      break;
    ts_target = timespec_add(ts_target, lb->duration);
  };
};

void *multi_peer_rate_test(struct peer *p, int count, int window) {
  struct timespec ts;
  pthread_t threadid;
  struct logbuffer lb;
  struct log_record lr;
  struct peer *sender;
  int sent = 0;
  int received = 0;
  int target;
  int RATEBLOCKSIZE = 1000000;
  logbuffer_init(&lb, 1000, RATEBLOCKSIZE, (struct timespec){1, 0});
  pthread_create(&threadid, NULL, (thread_t *)*logging_thread, &lb);
  gettime(&ts);
  clock_gettime(CLOCK_REALTIME, &lr.ts);
  lr.index = 0;
  logbuffer_write(&lb, &lr);

  sender = p + 1;
  do {
    target = window + received - sent;
    if (target > 0 && sent < count) {
      send_next_update(sender);
      sent++;
      if ((++sender)->sock == 0)
        sender = p + 1;
      continue;
    } else {
      if (bgp_receive(p)) {
        received++;
        if (0 == received % RATEBLOCKSIZE) {
          // ideally this should use a clock value from the read buffer system....
          clock_gettime(CLOCK_REALTIME, &lr.ts);
          lr.index = received / RATEBLOCKSIZE;
          logbuffer_write(&lb, &lr);
        }
      } else
        // bgp_receive() returned an exception
        break;
    };
  } while (received < count);
  // let the logger thread know it should exit.
  lr.ts = (struct timespec){0, 0};
  lr.index = -1;
  logbuffer_write(&lb, &lr);
  fprintf(stderr, "multi_peer_rate_test(%d/%d) transmit complete: elapsed time %s\n", count, window, showdeltams(ts));
  pthread_join(threadid, NULL);
  fprintf(stderr, "multi_peer_rate_test(%d) complete: elapsed time %s\n", count, showdeltams(ts));
};
