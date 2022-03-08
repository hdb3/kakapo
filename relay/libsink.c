/* kakapo-libsink - stripped down librelay
   run till completion a BGP session, and return the statistics
*/

#include "librelay.h"
#include "util.h"
#define FLAGS(a, b, c)

#define BUFSIZE (1024 * 1024 * 64)
#define MINREAD 4096

int peer_count = 0;
int nfds = 0;
int running = 0;
int listen_sock = -1;

int setupIOVECs(struct iovec *vec, void *base, uint64_t start, uint64_t end) {

  if ((end / BUFSIZE) > (start / BUFSIZE) && (end % BUFSIZE) != 0) {
    vec[0].iov_base = base + (start % BUFSIZE);
    vec[0].iov_len = BUFSIZE - (start % BUFSIZE);
    vec[1].iov_base = base;
    vec[1].iov_len = end % BUFSIZE;
    return 2;
  } else {
    vec[0].iov_base = base + (start % BUFSIZE);
    vec[0].iov_len = end - start;
    vec[1].iov_base = NULL;
    vec[1].iov_len = 0;
    return 1;
  };
};

int write_from(struct peer *p, int sock, int length) {
  int res;
  int niovec;
  struct iovec iovecs[2];
  while (length > 0) {
    niovec = setupIOVECs(iovecs, p->buf, p->nwrite, p->nwrite + length);
    res = writev(sock, iovecs, niovec);
    if (res == -1) {
      running = 0;
      printf("write error, errno: %d (%d)(%s)\n", errno, res, strerror(errno));
      return 0;
    } else {
      p->nwrite += res;
      length -= res;
    }
  }
  return 1;
};

void flush(struct peer *p) {
  if (p->nwriteable > 0) {
    write_from(p, listen_sock, p->nwriteable);
    p->nwriteable = 0;
  }
};

void forward(struct peer *p, int length) {
  p->nwriteable += length;
};

void echo(struct peer *p, int length) {
  flush(p);
  write_from(p, p->sock, length);
};

void stop(struct peer *p, int length) {
  flush(p);
  p->nwrite += length;
  running = 0;
  printf("stopping\n");
};

void processaction(struct peer *p);
int readaction(struct peer *p) {
  int res;
  int niovec;
  struct iovec iovecs[2];

  niovec = setupIOVECs(iovecs, p->buf, p->nread, p->nwrite + BUFSIZE);
  res = readv(p->sock, iovecs, niovec);

  if (res > 0) {
    p->nread += res;
    processaction(p);
  } else {
    // all other alternatives are terminal for this peer
    p->active = 0;
    running--;
    if (res == 0) {
      // normal end-of-stream
      printf("peer %d: end of stream on fd %d\n", p->peer_index, p->sock);
    } else {
      // some kind of error
      if (errno == EAGAIN) {
        ; // (errno == EAGAIN)
          // nothing available but not an error
          // should not happen as we only read if the pselect showed read will succeed
          // **** HOWEVER *******
          // experience shows that this does occur at the end of sessions
          // _repeatedly_
          // so, until resolved, it is best not to log it

      } else {
        printf("peer %d: end of stream on fd %d, errno: %d\n", p->peer_index, p->sock, errno);
      }
    }
  }
};

uint8_t *get_byte(struct peer *p, uint64_t offset) {
  return (p->buf) + ((offset + p->nprocessed) % BUFSIZE);
};

void dpi(struct peer *p, uint8_t msg_type, uint16_t msg_length) {
  // printf("dpi (%d)\n", p->peer_index);
  // this function is the opportunity to do as much or as little mangling as you might need
  // but it is inline with IO so best not take too long.....
  // for heavy processing it might be better to copy to a contiguous buffer
  // and return immediately
  // NB note that the buffer may be discontinuous when wrap-around occurs
  (5 > msg_type) || (1 < msg_type) || die("bad sync");
  (4097 > msg_length) || (19 < msg_length) || die("bad sync2");
  (*(p->msg_counts))++;
  (*(p->msg_counts + msg_type))++;
  switch (msg_type) {
  case 1:
    printf("peer %d, Open\n", p->peer_index);
    echo(p, msg_length);
    break;
  case 2:
    // printf("peer %d, Update\n", p->peer_index);
    if (msg_length == 23) {
      printf("peer %d, EoR\n", p->peer_index);
      echo(p, msg_length);
    }
    // forward(p, msg_length);
    break;
  case 3:
    printf("peer %d, Notification\n", p->peer_index);
    stop(p, msg_length);
    break;
  case 4:
    // printf("peer %d, Keepalive\n", p->peer_index);
    echo(p, msg_length);
    break;
  };
};

void processaction(struct peer *p) {
  // need at least 19 bytes to have a valid length field and a message type in a message....
  uint64_t available;
  available = p->nread - p->nprocessed;
  while (18 < available) {
    // TDOD could/should check for MARKER here....
    uint16_t msg_length = (*(get_byte(p, 16)) << 8) + *get_byte(p, 17);
    uint8_t msg_type = *get_byte(p, 18);
    if (msg_length <= available) {
      dpi(p, msg_type, msg_length);
      p->nprocessed += msg_length;
      available -= msg_length;
    } else {

      break;
    };
  };
  flush(p);
};

void sink(struct peer *p) {
  int res, n, flag;
  printf("libsink\n");
  p->buf = malloc(BUFSIZE);
  struct timeval timeout = {0, 100000}; // 100mS

  setsockopt(p->sock, SOL_SOCKET, SO_RCVTIMEO, (const char *)&timeout, sizeof timeout);

  running = 1;
  while (running) {
    readaction(p);
  };
  close(p->sock);
  free(p->buf);
  printf("libsink done\n");
};

void peer_report(struct peer *p) {
  printf("                                        bytes                 messages\n");
  printf("peer# local            remote           read       written    Total    Opens Updates  Notifications Keepalives\n");
  printf(" %2d   %-16s ", p->peer_index, inet_ntoa(p->local.sin_addr));
  printf("%-16s ", inet_ntoa(p->remote.sin_addr));
  printf("%-10ld %-10ld ", p->nread, p->nwrite);
  printf("%-8d ", *(p->msg_counts));
  printf("%-6d", (p->msg_counts)[1]);
  printf("%-8d ", (p->msg_counts)[2]);
  printf("%-13d ", (p->msg_counts)[3]);
  printf("%d \n", (p->msg_counts)[4]);
}