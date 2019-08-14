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
  // uint16_t tpal = ntohs(*(uint16_t *)(update.data + wrl + 2));
  // assert(wrl + tpal < update.length - 3);
  // char *nlri = update.data + wrl + tpal + 4;
  // uint16_t nlril = update.length - wrl - tpal - 4;
  if (0 == wrl)
    return (struct bytestring){0, 0};
  else
    return (struct bytestring){wrl, update.data};
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
  int i;
  struct bytestring *vec = malloc(sizeof(struct bytestring) * length);
  int buflen = 0;

  for (i = 0; i < length; i++) {
    // int seq = i % TABLESIZE;
    // int cyclenumber = 1 + i / TABLESIZE;
    struct bytestring b = update(nlris(SEEDPREFIX, SEEDPREFIXLEN, GROUPSIZE, i),
                                 empty,
                                 iBGPpath(localip,
                                          localpref,
                                          (uint32_t[]){i + TEN7, peer_index, TEN7 + usn / TEN7, TEN7 + usn % TEN7, 0}));
    // struct bytestring b = update(nlris(SEEDPREFIX, SEEDPREFIXLEN, GROUPSIZE, usn), empty, iBGPpath(localip, (uint32_t[]){usn + 1000000, cyclenumber, 0}));
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
  return (struct bytestring){buflen, data};
};

void send_update_block(int offset, int length, struct peer *p) {

  // it appears the bgpd (openBGP) wants keepalives even if it is getting Updates!!!
  _send(p, keepalive, 19);
  uint32_t localpref = ((0 == length) ? ((0 == usn) ? 100 : 99) : 101 + usn % TABLESIZE);
  struct bytestring updates = build_update_block(p->tidx, ((0 == length) ? TABLESIZE : length), p->localip, localpref);
  _send(p, updates.data, updates.length);
  txwait(p->sock);
  free(updates.data);
};

void send_single_update(struct peer *p, uint32_t ip, uint8_t length) {
  struct bytestring b = update(nlris(ip, length, 1, 0), empty, iBGPpath(p->localip, 100, (uint32_t[]){p->tidx, 0}));
  _send(p, b.data, b.length);
  free(b.data);
};

void send_single_withdraw(struct peer *p, uint32_t ip, uint8_t length) {
  struct bytestring b = update(empty, nlris(ip, length, 1, 0), empty);
  _send(p, b.data, b.length);
  free(b.data);
};

void advertise_canary(struct peer *p) {
  send_single_update(p, CANARYSEED + __bswap_32(p->tidx), 32);
};

void withdraw_canary(struct peer *p) {
  send_single_withdraw(p, CANARYSEED + __bswap_32(p->tidx), 32);
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

typedef int(pf_t)(void *, struct bgp_message *);
typedef void *(thread_t)(void *);

struct crf_state {
  struct timespec start;
  struct timespec end;
  int status;
};

void crf(struct crf_state *crfs, int (*pf)(void *, struct bgp_message *), void *pf_state, struct peer *p) {

  struct bgp_message bm;
  crfs->status = 0;

  gettime(&crfs->start);
  while (crfs->status == 0) {
    getBGPMessage(&bm, &(p->sb));
    if (BGPKEEPALIVE == bm.msgtype)
      break;
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
  // fprintf(stderr, "crf() completed in %ld\n", timespec_to_ms(timespec_sub(crfs->end, crfs->start)));
};

int pf_withdrawn(int *counter, struct bgp_message *bm) {
  struct bytestring msg = (struct bytestring){bm->pl, bm->payload};
  struct bytestring withdrawn = get_withdrawn(msg);
  int withdrawn_count = nlri_count(withdrawn);
  printf("** pf_withdrawn - %d/%d\n", *counter, withdrawn_count);
  (*counter) -= withdrawn_count;
  return (*counter > 0 ? 0 : 1);
};

void crf_withdrawn_count(int counter, struct crf_state *crfs, struct peer *p) {
  int counter_end = counter;
  printf("** START - crf_withdrawn_count - %d\n", counter_end);
  crf(crfs, (pf_t *)pf_withdrawn, &counter_end, p);
  if (1 != crfs->status)
    printf("** WARNING - crf_withdrawn_count - counter start: %d counter end: %d\n", counter, counter_end);
  else
    printf("** END - crf_withdrawn_count - %d\n", counter);
};

int pf_update(struct prefix *pfx, struct bgp_message *bm) {
  struct bytestring msg = (struct bytestring){bm->pl, bm->payload};
  struct bytestring nlri = get_nlri(msg);
  return (nlri_member(nlri, *pfx));
};

void crf_update(struct prefix *pfx, struct crf_state *crfs, struct peer *p) {
  return crf(crfs, (pf_t *)pf_update, pfx, p);
};

int pf_count(int *counter, struct bgp_message *bm) {
  (*counter)--;
  return (*counter > 0 ? 0 : 1);
};

void crf_count(int counter, struct crf_state *crfs, struct peer *p) {
  int counter_start = counter;
  int counter_end = counter;
  crf(crfs, (pf_t *)pf_count, &counter_end, p);
  if (1 != crfs->status)
    printf("** WARNING - crf_count - counter start: %d counter end: %d\n", counter_start, counter_end);
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
  // sleep(5);
  pthread_join(crf_threadid, NULL);
  fprintf(stderr, "crf_canary_test total elapsed time %s\n", showdeltams(ts));
};

void *crf_thread(struct peer *p) {
  struct timespec ts;
  struct crf_state crfs;
  fprintf(stderr, "crf_test listener : %s\n", fromHostAddress(p->localip));
  gettime(&ts);
  crf_count(TABLESIZE, &crfs, p);
  fprintf(stderr, "crf_test listener return status=%d elapsed time %s\n", crfs.status, showdeltams(ts));
};

void *crf_test(struct peer *p) {
  struct timespec ts;
  pthread_t crf_threadid;

  pthread_create(&crf_threadid, NULL, (thread_t *)crf_thread, p);

  fprintf(stderr, "crf_test sender   : %s\n", fromHostAddress((p + 1)->localip));
  gettime(&ts);
  // sleep(5);
  send_update_block(0, TABLESIZE, p + 1);
  fprintf(stderr, "crf_test transmit elapsed time %s\n", showdeltams(ts));
  pthread_join(crf_threadid, NULL);
  fprintf(stderr, "crf_test total elapsed time %s\n", showdeltams(ts));
};

void *single_peer_burst_test(struct peer *p) {
  struct crf_state crfs;
  struct timespec ts;

  fprintf(stderr, "single_peer_burst_test listener : %s\n", fromHostAddress(p->localip));
  fprintf(stderr, "single_peer_burst_test sender   : %s\n", fromHostAddress((p + 1)->localip));
  gettime(&ts);

  send_update_block(0, TABLESIZE, p + 1);
  fprintf(stderr, "single_peer_burst_test transmit elapsed time %s\n", showdeltams(ts));
  crf_count(TABLESIZE, &crfs, p);
  fprintf(stderr, "single_peer_burst_test(%d) return status=%d elapsed time %s\n", TABLESIZE, crfs.status, showdeltams(ts));
};

void *canary(struct peer *p) {
  struct timespec ts;
  struct crf_state crfs;
  int i = 1;

  gettime(&ts);
  while ((p + i)->sock != 0) {
    // fprintf(stderr, "canary from : %s\n", fromHostAddress((p+i)->localip));
    advertise_canary(p + i);
    i++;
  };
  int sender_count = i - 1;
  // fprintf(stderr, "%d sender peers found n", sender_count);
  crf_count(sender_count, &crfs, p);
  // fprintf(stderr, "first canary replies complete: elapsed time %s\n", showdeltams(ts));

  i = 1;
  while (i <= sender_count) {
    // fprintf(stderr, "canary from : %s\n", fromHostAddress((p+i)->localip));
    withdraw_canary(p + i);
    i++;
  };
  crf_withdrawn_count(sender_count, &crfs, p);
  // fprintf(stderr, "final canary replies complete: elapsed time %s\n", showdeltams(ts));
  fprintf(stderr, "canary complete: elapsed time %s\n", showdeltams(ts));
};

void *strict_canary(struct peer *listener, struct peer *p) {
  struct timespec ts;
  struct crf_state crfs;
  struct prefix pfx = (struct prefix){CANARYSEED + __bswap_32(p->tidx), 32};

  gettime(&ts);
  fprintf(stderr, "canary from : %s\n", fromHostAddress(p->localip));
  send_single_update(p, CANARYSEED + __bswap_32(p->tidx), 32);
  crf_update(&pfx, &crfs, listener);
  // crf_count(1, &crfs, p);
  fprintf(stderr, "first canary reply complete: elapsed time %s\n", showdeltams(ts));

  fprintf(stderr, "canary wthdraw from : %s\n", fromHostAddress(p->localip));
  // withdraw_canary(p);
  send_single_withdraw(p, CANARYSEED + __bswap_32(p->tidx), 32);
  //crf_count(1, &crfs, p);
  crf_withdrawn_count(1, &crfs, listener);
  //fprintf(stderr, "final canary repliy complete: elapsed time %s\n", showdeltams(ts));

  fprintf(stderr, "canary complete: elapsed time %s\n", showdeltams(ts));
};

void *conditioning_single_peer(struct peer *p) {
  struct timespec ts;

  gettime(&ts);
  fprintf(stderr, "conditioning from : %s\n", fromHostAddress(p->localip));
  send_update_block(0, TABLESIZE, p);
  fprintf(stderr, "conditioning complete: elapsed time %s\n", showdeltams(ts));
};

void *conditioning(struct peer *p) {
  struct timespec ts;
  struct peer *listener = p;
  gettime(&ts);
  while ((++p)->sock != 0) {
    fprintf(stderr, "conditioning from : %s\n", fromHostAddress(p->localip));
    send_update_block(0, TABLESIZE, p);
    strict_canary(listener, p);
  };
  fprintf(stderr, "conditioning complete: elapsed time %s\n", showdeltams(ts));
};

void *notify_all(struct peer *p) {
  struct timespec ts;

  gettime(&ts);
  while ((p)->sock != 0) {
    fprintf(stderr, "Notification from : %s\n", fromHostAddress(p->localip));
    send_notification(p, NOTIFICATION_CEASE, NOTIFICATION_ADMIN_RESET);
    p++;
  };
  fprintf(stderr, "Notification complete: elapsed time %s\n", showdeltams(ts));
};
