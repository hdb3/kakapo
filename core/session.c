/* kakapo-session - a BGP traffic source and sink */

#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <linux/sockios.h>
#include <net/if.h>
#include <netinet/tcp.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/sendfile.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <time.h>
#include <unistd.h>

#include "kakapo.h"
#include "libutil.h"

#define BUFFSIZE 0x10000
#define LOG_BUFFER_SIZE 1000
#define LOG_CYCLE_DURATION 1 // units: seconds

#define NOTIFICATION_CEASE 6
#define NOTIFICATION_ADMIN_RESET 4

// AS_TRANS - see RFC4893
#define AS_TRANS 23456

// wrapper for send which protects against multiple thread writes interleaving output
// the protection is 'soft' in that it introduces wait until queue empty semantics
// rather than an explicit semaphore

// __send: variant od _send which does not use txwait
void __send(struct peer *p, const void *buf, size_t count) {
  size_t sent, total_sent;
  if (p->sendFlag != 0)
    die("send flag reentry fail");
  else
    p->sendFlag = 1;
  total_sent = 0;
  do {
    sent = send(p->sock, buf + total_sent, count - total_sent, 0);
    if (sent == -1)
      perror("send fail");
    assert(sent != -1);
    total_sent += sent;
    if (total_sent < count)
      printf("******total_sent<count: %ld %ld\n", total_sent, count);
  } while (total_sent < count);
  p->sendFlag = 0;
};

void _send(struct peer *p, const void *buf, size_t count) {
  int sent;
  txwait(p->sock);
  __send(p, buf, count);
  txwait(p->sock);
};

void send_from_file(struct peer *p, char *fname) {
  struct stat statbuf;
  if (p->sendFlag != 0)
    die("send flag reentry fail");
  else
    p->sendFlag = 1;
  fprintf(stderr, "send_from_file using %s, in working directory %s\n", fname, get_current_dir_name());
  int fd = open(fname, 0);
  if (fd == -1) {
    die("open file fail");
  };
  assert(fstat(fd, &statbuf) != -1);
  long to_send = statbuf.st_size;
  long sent = 0;
  fprintf(stderr, "opened %s, filesize is %ld\n", fname, to_send);
  while (to_send > 0) {
    sent = sendfile(p->sock, fd, 0, to_send);
    if (-1 == sent)
      die("trouble in sendfile");
    else
      to_send -= sent;
  };
  assert(0 == to_send);
  p->sendFlag = 0;
};

unsigned char notification[21] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0, 21, 3, 0, 0};
void send_notification(struct peer *p, unsigned char major, unsigned char minor) {
  notification[19] = major;
  notification[20] = minor;
  __send(p, notification, 21);
  close(p->sock);
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

uint16_t doopen(char *msg, int length) {
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
  return as;
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

// TODO inline these...?
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
      if ((0 == bm->payload) || ((char *)-1 == bm->payload)) {
        fprintf(stderr, "unexpected end of stream after header received\n");
        bm->msgtype = BGPENDOFSTREAM;
      }
    }
  }
}

static uint64_t usn = 0;
#define TEN7 10000000
uint32_t *usn_path(int tidx) {
  usn++;
  return aspathbuild((uint32_t)tidx, TEN7 + usn % TEN7, TEN7 + usn / TEN7, 0);
}

static void *_build_update_block_buf = NULL;
static size_t _build_update_block_siz = 0;

struct bytestring build_update_block(int peer_index, int length, uint32_t localip, uint32_t localpref, bool isEBGP) {

  assert(length <= TABLESIZE);
  uint64_t i;
  struct bytestring *vec = malloc(sizeof(struct bytestring) * length);
  uint64_t buflen = 0;
  char *data;

  for (i = 0; i < length; i++) {
    uint32_t *path = usn_path(peer_index);

    struct bytestring b = update(
        nlris(SEEDPREFIX, SEEDPREFIXLEN, GROUPSIZE, usn % TABLESIZE),
        empty,
        isEBGP ? eBGPpath(localip, localpref + usn / TABLESIZE, path)
               : iBGPpath(localip, localpref + usn / TABLESIZE, path));
    vec[i] = b;
    buflen += b.length;
  };

  if (_build_update_block_siz >= buflen)
    data = _build_update_block_buf;
  else {
    if (NULL == _build_update_block_buf)
      data = malloc(buflen);
    else
      data = realloc(_build_update_block_buf, buflen);
    _build_update_block_buf = data;
    _build_update_block_siz = buflen;
  };
  assert(NULL != data);

  char *offset = data;
  // TDOD - there seems to be some unnecessary mallocing and freeing here,
  //        why is the update set not built directly into the output buffer

  for (i = 0; i < length; i++) {
    offset = mempcpy(offset, vec[i].data, vec[i].length);
    free(vec[i].data);
  };
  free(vec);
  return (struct bytestring){buflen, data};
};

void send_update_block(int length, struct peer *p) {
  bool isEBGP = (p->as != p->remoteas);
  uint32_t localpref = ((0 == length) ? ((0 == usn) ? 100 : 99) : 101 + usn / TABLESIZE);
  struct bytestring updates = build_update_block(p->tidx, ((0 == length) ? TABLESIZE : length), p->localip, localpref, isEBGP);
  __send(p, updates.data, updates.length);
};

void send_next_update(struct peer *p) {
  send_update_block(1, p);
};

void send_single_update(struct peer *p, struct prefix *pfx) {
  bool isEBGP = (p->as != p->remoteas);
  uint32_t *path = usn_path(p->tidx);

  struct bytestring b = update(
      nlris(pfx->ip, pfx->length, 1, 0),
      empty,
      isEBGP ? eBGPpath(p->localip, 100, path)
             : iBGPpath(p->localip, 100, path));
  _send(p, b.data, b.length);
  free(b.data);
};

void send_single_withdraw(struct peer *p, struct prefix *pfx) {
  struct bytestring b = update(empty, nlris(pfx->ip, pfx->length, 1, 0), empty);
  _send(p, b.data, b.length);
  free(b.data);
};

void send_eor(struct peer *p) {

  struct bytestring b = update(empty, empty, empty);
  _send(p, b.data, b.length);
  free(b.data);
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
    uint16_t remoteas = doopen(bm.payload, bm.pl);
    p->remoteas = remoteas;
    if (remoteas == AS_TRANS) {
      fprintf(stderr, "** NOTICE - PEER sent AS_TRANS in BGP OPEN - probably harmless - assume as eBGP\n");
    }
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
  pf_t *pf;
  struct peer *p;
  void *pf_state;
};

void crf(struct crf_state *crfs, pf_t(*pf), void *pf_state, struct peer *p) {

  struct bgp_message bm;
  crfs->status = 0;

  gettime(&crfs->start);
  while (crfs->status == 0) {
    getBGPMessage(&bm, &(p->sb));
    if (BGPKEEPALIVE == bm.msgtype)
      continue;
    else if (BGPUPDATE == bm.msgtype) {
      // ignore EOR
      if (bm.pl == 4)
        doeor(bm.payload);
      else
        crfs->status = pf(pf_state, &bm);
      continue;
    } else if (BGPTIMEOUT == bm.msgtype) {
      fprintf(stderr, "crf: timeout\n");
      crfs->status = -2;
    } else {
      fprintf(stderr, "crf: exception exit\n");
      report(BGPUPDATE, bm.msgtype);
      crfs->status = -1;
    }
  };
  crfs->end = p->sb.rcvtimestamp;
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

void crf_update(struct prefix *pfx, struct crf_state *crfs, struct peer *p) {
  crf(crfs, (pf_t *)pf_update, pfx, p);
};

int pf_count(int *counter, struct bgp_message *bm) {
  struct bytestring msg = (struct bytestring){bm->pl, bm->payload};
  struct bytestring nlri = get_nlri(msg);
  int n_of_nlris = nlri_count(nlri);
  (*counter) -= n_of_nlris;
  return (*counter > 0 ? 0 : 1);
};

void *crfw(struct crf_state *crfs) {
  crf(crfs, crfs->pf, crfs->pf_state, crfs->p);
};

pthread_t crf_count(int *counter, struct crf_state *crfs, struct peer *p) {
  pthread_t threadid;
  crfs->p = p;
  crfs->pf = (void *)pf_count;
  crfs->pf_state = counter;
  pthread_create(&threadid, NULL, (void *)crfw, crfs);
  return threadid;
};

void establish(void *x) {
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

  // send_eor(p);

  pthread_exit(NULL);

exit:
  fprintf(stderr, "establish: abnormal exit\n");
};

double single_peer_burst_test(uint32_t count) {
  struct crf_state crfs;
  struct timespec tx_start, tx_end;
  double tx_elapsed, rx_elapsed, elapsed;
  int n = count * GROUPSIZE; // need to count prefixes sent/received, not updates

  memset(&crfs, 0, sizeof(struct crf_state));
  gettime(&tx_start);

  pthread_t threadid = crf_count(&n, &crfs, listener);
  send_update_block(count, senders);
  txwait(senders->sock);
  gettime(&tx_end);
  _pthread_join(threadid);

  tx_elapsed = timespec_to_double(timespec_sub(tx_end, tx_start));
  rx_elapsed = timespec_to_double(timespec_sub(crfs.end, crfs.start));
  elapsed = timespec_to_double(timespec_sub(crfs.end, tx_start));

  fprintf(stderr, "single_peer_burst_test total elapsed time %f count %d", elapsed, count * GROUPSIZE);
  fprintf(stderr, " (transmit %f)", tx_elapsed);

  if (1 == crfs.status)
    fprintf(stderr, " (receive %f)\n", rx_elapsed);
  else if (-2 == crfs.status)
    fprintf(stderr, " (receive ** TIMEOUT **  elapsed time %f  dropped %d/%d)\n", rx_elapsed, n, count * GROUPSIZE);
  else
    fprintf(stderr, " ( receive ** EXCEPTION CODE %d **  elapsed time %f  dropped %d/%d)\n", crfs.status, rx_elapsed, n, count * GROUPSIZE);

  return elapsed;
};

static struct prefix _pfx;
struct prefix *canary_base() {
  _pfx = (struct prefix){CANARYSEED, 32};
  return &_pfx;
};

struct prefix *canary_peer(struct peer *p) {
  _pfx = (struct prefix){CANARYSEED + __bswap_32(p->tidx), 32};
  return &_pfx;
};

void canary(struct peer *p) {
  struct crf_state crfs;

  fprintf(stderr, "canary for %s\n", show_peer(p));
  send_single_update(p, canary_peer(p));
  crf_update(canary_peer(p), &crfs, listener);
};

struct rx_data {
  struct peer *target;
  struct peer *listener;
  pthread_t threadid;
};

void rx_thread(struct rx_data *rxd) {
  struct crf_state crfs;
  crf_update(canary_peer(rxd->target), &crfs, rxd->listener);
};

struct rx_data *rx_start(struct peer *target, struct peer *listener) {
  struct rx_data *rxd = calloc(1, sizeof(struct rx_data));

  rxd->target = target;
  rxd->listener = listener;
  rxd->threadid = _pthread_create((void *)rx_thread, rxd);
  return rxd;
};

void rx_end(struct rx_data *rxd) {
  send_single_update(rxd->target, canary_peer(rxd->target));
  _pthread_join(rxd->threadid);
  free(rxd);
};

void sendfile_single_peer(struct peer *target, char *fname) {
  struct timespec ts;

  struct rx_data *rxd = rx_start(target, listener);
  gettime(&ts);
  send_from_file(target, fname);
  rx_end(rxd);
};

void conditioning_single_peer(struct peer *target) {
  struct timespec ts;

  struct rx_data *rxd = rx_start(target, listener);
  gettime(&ts);
  fprintf(stderr, "conditioning:          %s\n", show_peer(target));
  send_update_block(TABLESIZE, target);
  send_eor(target);
  rx_end(rxd);
  fprintf(stderr, "conditioning complete: %s  elapsed time %s\n", show_peer(target), showdeltats(ts));
};

void keepalive_all() {
  int i;
  for (i = 0; i < peer_count; i++)
    send_keepalive(peertable + i);
};

void canary_all() {
  struct timespec ts;
  int i;
  gettime(&ts);
  for (i = 0; i < sender_count; i++)
    canary(senders + i);
  fprintf(stderr, "canary_all complete: elapsed time %s\n", showdeltats(ts));
};

double multi_peer_burst_test(uint32_t count) {
  struct crf_state crfs;
  memset(&crfs, 0, sizeof(struct crf_state));
  struct timespec tx_start, tx_end;

  double tx_elapsed, rx_elapsed, elapsed;

  pthread_t threadid;
  struct peer *sender;
  int sent = 0;
  int i = 0;
  int n = count * GROUPSIZE; // need to count prefixes sent/received, not updates
  threadid = crf_count(&n, &crfs, listener);
  gettime(&tx_start);
  while (sent < count) {
    send_next_update(senders + i);
    sent++;
    i = (++i < sender_count) ? i : 0;
  };

  for (int i = 0; i++; i < sender_count)
    txwait((senders + i)->sock);

  gettime(&tx_end);

  _pthread_join(threadid);
  // TODO use getdeltats() like conditioning does...

  tx_elapsed = timespec_to_double(timespec_sub(tx_end, tx_start));
  rx_elapsed = timespec_to_double(timespec_sub(crfs.end, crfs.start));
  elapsed = timespec_to_double(timespec_sub(crfs.end, tx_start));

  fprintf(stderr, "multi_peer_burst_test(%d) complete: elapsed time %f (transmit %f) count %d\n", sender_count, elapsed, tx_elapsed, count * GROUPSIZE);

  if (1 == crfs.status)
    fprintf(stderr, " (receive %f)\n", rx_elapsed);
  else if (-2 == crfs.status)
    fprintf(stderr, " (receive ** TIMEOUT **  elapsed time %f  dropped %d/%d)\n", rx_elapsed, n, count * GROUPSIZE);
  else
    fprintf(stderr, " ( receive ** EXCEPTION CODE %d **  elapsed time %f  dropped %d/%d)\n", crfs.status, rx_elapsed, n, count * GROUPSIZE);

  return elapsed;
};

double file_test(char *fname) {
  int i;
  struct timespec ts;
  double elapsed;
  gettime(&ts);
  fprintf(stderr, "file_test start\n");
  sendfile_single_peer(senders, fname);
  send_eor(senders);
  canary(senders);
  elapsed = getdeltats(ts);
  fprintf(stderr, "file_test complete: elapsed time %f\n", elapsed);
  return elapsed;
};

double conditioning() {
  int i;
  struct timespec ts;
  double elapsed;
  gettime(&ts);
  fprintf(stderr, "conditioning start\n");
  for (i = 0; i < sender_count; i++) {
    conditioning_single_peer(senders + i);
    keepalive_all(); // protect against keepalive timeout failure in large configs
  };
  elapsed = getdeltats(ts);
  fprintf(stderr, "conditioning complete: elapsed time %f\n", elapsed);
  return elapsed;
};

void notify_all() {
  int i;
  struct timespec ts;
  gettime(&ts);
  for (i = 0; i < peer_count; i++)
    send_notification(peertable + i, NOTIFICATION_CEASE, NOTIFICATION_ADMIN_RESET);
  fprintf(stderr, "Notification complete: elapsed time %s\n", showdeltats(ts));
};

//
// Continuous mode test support
//

static struct bgp_message _bm;

struct bgp_message *bgp_receive(struct peer *p) {

  while (1) {
    getBGPMessage(&_bm, &(p->sb));
    if (BGPKEEPALIVE == _bm.msgtype)
      continue;
    if (BGPUPDATE == _bm.msgtype) {
      // ignore EOR
      if (_bm.pl == 4)
        continue;
      else
        return &(_bm);
    } else {
      report(BGPUPDATE, _bm.msgtype);
      return NULL;
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

  // **** usefull diagnostic!!!!
  // do not remove!!!!!
  printf("sent/received: %d/%d\r", lb->sent, lb->received);
  fflush(stdout);

  while (NULL != lrp) {
    if (0 == lrp->ts.tv_sec)
      return 1;
    else {
      if (0 == lrp->index) { // first entry
        llp->first_lr = *lrp;
        llp->last_lr = *lrp;
      } else { // 2nd or later entry, rate calculation is possible
        struct timespec aggregate_duration = timespec_sub(lrp->ts, llp->first_lr.ts);

        // 'aggregate_count'- why is there not simply a running count?
        int aggregate_count = lrp->index * lb->block_size;
        struct timespec cycle_duration = timespec_sub(lrp->ts, llp->last_lr.ts);

        // 'cycle_count'- why is there not simply a running count?
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

void logging_thread(struct logbuffer *lb) {

  struct timespec ts_delay, ts_target, ts_entry, ts_exit, ts_now;
  struct logger_local *llp = malloc(sizeof(struct logger_local));

  gettime(&ts_target);

  while (1) {
    gettime(&ts_now);
    if (ts_now.tv_sec > lb->deadline.tv_sec)
      lb->stop_flag = true;

    ts_delay = timespec_sub(ts_target, ts_now);
    while (ts_delay.tv_sec > 0 && ts_delay.tv_nsec > 0)
      if (0 == nanosleep(&ts_delay, &ts_delay))
        break;
    if (logger(lb, llp))
      break;
    ts_target = timespec_add(ts_target, lb->log_cycle_duration);
  };
};

int rate_test(uint32_t nsenders, uint32_t count, uint32_t window) {
  struct timespec ts;
  double elapsed;
  pthread_t threadid;
  struct logbuffer lb;
  struct log_record lr;
  struct peer *sender;
  int target;
  int blocking_factor;

  assert(nsenders > 0 && nsenders <= sender_count);
  struct timespec deadline;
  gettime(&deadline);
  deadline.tv_sec += RATETIMELIMIT;
  struct timespec log_cycle_duration = {.tv_sec = LOG_CYCLE_DURATION};

  logbuffer_init(&lb, LOG_BUFFER_SIZE, RATEBLOCKSIZE, log_cycle_duration, deadline);
  pthread_create(&threadid, NULL, (thread_t *)*logging_thread, &lb);
  gettime(&ts);
  clock_gettime(CLOCK_REALTIME, &lr.ts);
  lr.index = 0;
  logbuffer_write(&lb, &lr);

  fprintf(stderr, "rate_test start, % d sending peers\n", nsenders);
  keepalive_all(); // absent a separate keep alive thread we need to take this precaution
  sender = senders;
  do {
    target = window + lb.received - lb.sent;
    if (target > 0 && lb.sent < count) {
      blocking_factor = 1 + target / peer_count;
      blocking_factor = (blocking_factor > MAXBLOCKINGFACTOR ? MAXBLOCKINGFACTOR : blocking_factor);
      send_update_block(blocking_factor, sender++);
      lb.sent += blocking_factor;
      if (sender = senders + nsenders)
        sender = senders;
      // keep sending until the window is full
      continue;
    } else
      do {
        if (bgp_receive(listener)) {
          lb.received++;
          if (0 == lb.received % RATEBLOCKSIZE) {
            lr.ts = listener->sb.rcvtimestamp;

            lr.index = lb.received / RATEBLOCKSIZE;
            logbuffer_write(&lb, &lr);
            keepalive_all(); // absent a separate keep alive thread we need to take this precaution
                             // in case of long running rate tests
          }
        } else
          // bgp_receive() returned an exception
          goto terminate;
        // break;
        // keep reading while the current read buffer is not exhausted
      } while (bgp_peek(&listener->sb));
  } while (lb.received < count && !lb.stop_flag);
terminate:
  // let the logger thread know it should exit.
  lr.ts = (struct timespec){0, 0};
  lr.index = -1;
  logbuffer_write(&lb, &lr);
  _pthread_join(threadid);

  // this is the elapsed time even if there was an exception
  // based on the last succesful receive
  elapsed = timespec_to_double(timespec_sub(listener->sb.rcvtimestamp, ts));
  int _count = lb.received;
  fprintf(stderr, "rate_test(%d/%d) transmit complete: elapsed time %0.3f, rate %d msgs/sec\n", _count, window, elapsed, (int)(_count / elapsed));

  return (int)(_count / elapsed);
};

int multi_peer_rate_test(uint32_t count, uint32_t window) {
  return rate_test(sender_count, count, window);
};

int single_peer_rate_test(uint32_t count, uint32_t window) {
  return rate_test(1, count, window);
};

// ******* functional test extensions

void func_test(uint32_t nsenders, uint32_t count) {
  struct timespec ts;
  pthread_t threadid;
  struct logbuffer lb;
  struct log_record lr;
  struct peer *sender;
  struct bgp_message *bm;
  struct prefix *next_prefix;

  assert(nsenders > 0 && nsenders <= sender_count);
  logbuffer_init(&lb, LOG_BUFFER_SIZE, RATEBLOCKSIZE, (struct timespec){LOG_CYCLE_DURATION, 0}, (struct timespec){100, 0});
  pthread_create(&threadid, NULL, (thread_t *)*logging_thread, &lb);
  gettime(&ts);
  clock_gettime(CLOCK_REALTIME, &lr.ts);
  lr.index = 0;
  logbuffer_write(&lb, &lr);

  fprintf(stderr, "func_test start, % d sending peers\n", nsenders);
  sender = senders;
  while (lb.received < count) {
    next_prefix = get_prefix_list(SEEDPREFIX, SEEDPREFIXLEN, GROUPSIZE, usn % TABLESIZE);
    send_update_block(1, sender++);
    lb.sent++;
    if (sender = senders + nsenders)
      sender = senders;
    bm = bgp_receive(listener);
    if (bm) {
      struct bytestring msg = (struct bytestring){bm->pl, bm->payload};
      struct bytestring nlri = get_nlri(msg);
      struct prefix pfx = get_prefix_nlri(nlri.data);

      if (cmp_prefix(next_prefix, &pfx)) {
        lb.received++;
        if (0 == lb.received % RATEBLOCKSIZE) {
          lr.ts = listener->sb.rcvtimestamp;
          lr.index = lb.received / RATEBLOCKSIZE;
          logbuffer_write(&lb, &lr);
        }
      } else {
        // exception comparing expect/got
        fprintf(stderr, "exception in func_test, expecting %s", showprefix(*next_prefix));
        fprintf(stderr, " , got %s, index %d\n", showprefix(pfx), count);
        break;
      }
    } else {
      // bgp_receive() returned an exception
      fprintf(stderr, "bgp_receive() returned an exception, expecting %s\n", showprefix(*next_prefix));
      break;
    }
    count++;
  };
  // let the logger thread know it should exit.
  lr.ts = (struct timespec){0, 0};
  lr.index = -1;
  logbuffer_write(&lb, &lr);
  _pthread_join(threadid);
  fprintf(stderr, "func_test(%d) complete: elapsed time %s\n", count, showdeltats(ts));
};

void multi_peer_func_test(uint32_t count) {
  func_test(sender_count, count);
};

void single_peer_func_test(uint32_t count) {
  func_test(1, count);
};
