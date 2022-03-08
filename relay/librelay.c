/* kakapo-librelay - a BGP traffic relay library
   intended use: as an FFI invoked function to provide similar or better functionality as
   the existing standalone applications, e.g. relay, relay2.
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

int flush(struct peer *p) {
  if (p->nwriteable > 0) {
    write_from(p, listen_sock, p->nwriteable);
    p->nwriteable = 0;
  }
};

int forward(struct peer *p, int length) {
  p->nwriteable += length;
};

int echo(struct peer *p, int length) {
  flush(p);
  write_from(p, p->sock, length);
};

int stop(struct peer *p, int length) {
  flush(p);
  p->nwrite += length;
  running = 0;
  printf("stopping\n");
};

int canRead(int fd, uint64_t nread, uint64_t nwrite) {
  int p = (MINREAD < BUFSIZE + nwrite - nread) ? 1 : 0;
  return p;
  if (p) {
    printf("fd%d - CAN READ %ld (%ld %ld)\n", fd, BUFSIZE + nwrite - nread, nread, nwrite);
  } else {
    printf("fd%d - CANNOT READ %ld (%ld %ld)\n", fd, BUFSIZE + nwrite - nread, nread, nwrite);
  };
  return p;
};

void processaction(struct peer *p);
int readaction(struct peer *p, int read_flag) {
  int res;
  int niovec;
  struct iovec iovecs[2];

  if (read_flag && canRead(p->sock, p->nread, p->nwrite)) {
    niovec = setupIOVECs(iovecs, p->buf, p->nread, p->nwrite + BUFSIZE);
    res = readv(p->sock, iovecs, niovec);

    if (res > 0) {
      p->nread += res;
      processaction(p);
      // TDOD this looks like duplication....
      // flush(p);
    } else {
      // all alternatives are terminal for this peer
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
            // should not happen as we only read if the pselect showed read will succceed
            // **** HOWEVER *******
            // experience shows that this does occur at the end of sessions
            // _repeatedly_
            // so, until resolved, it is best not to log it

        } else {
          printf("peer %d: end of stream on fd %d, errno: %d\n", p->peer_index, p->sock, errno);
        }
      }
    }

  } else // this is either the (unexpected) case where we cannot read because the buffer is full
         // or more likely simply that read flag is unset and this is our chance to set it again
    ;    // printf("peer %d: impossible invocation on fd %d\n", p->peer_index, p->sock);
  return canRead(p->sock, p->nread, p->nwrite);
};

uint8_t *get_byte(struct peer *p, uint64_t offset) {
  return (p->buf) + ((offset + p->nprocessed) % BUFSIZE);
};

void dpi(struct peer *p, uint8_t msg_type, uint16_t msg_length) {
  // this function is the opportunity to do as much or as little mangling as you might need
  // but it is inline with IO so best not take too long.....
  // for heavy processing it might be better to copy to a contiguous buffer
  // and return immediately
  // NB note that the buffer may be discontinuous when wrap-around occurs
  (5 > msg_type) || (1 < msg_type) || die("bad sync");
  (4097 > msg_length) || (19 < msg_length) || die("bad sync2");
  (*(p->msg_counts))++;
  (*(p->msg_counts + msg_type))++;
  // notice: if counts are zeroed out at connection time then the Open and Notification counts can be used as flags for state changes
  // an EndOfRIB detector would be simple too, by checking for specific msg_length and msg_type
  switch (msg_type) {
  case 1:
    printf("peer %d, Open\n", p->peer_index);
    echo(p, msg_length);
    break;
  case 2:
    // printf("peer %d, Update\n", p->peer_index);
    forward(p, msg_length);
    break;
  case 3:
    printf("peer %d, Notification\n", p->peer_index);
    stop(p, msg_length);
    break;
  case 4:
    printf("peer %d, Keepalive\n", p->peer_index);
    echo(p, msg_length);
    break;
  };
};

void processaction(struct peer *p) {
  // need at least 19 bytes to have a valid length field and a message type in a message....
  uint64_t available;
  // uncomment to remove message delineation
  // p->nprocessed = p->nread;
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

void relay_init(int peer_count, struct peer *peer_table) {
  for (int i = 0; i < peer_count; i++) {
    struct peer *p = peer_table + i;
    p->peer_index = i;
    p->active = 1;
    p->buf = malloc(BUFSIZE);
    nfds = p->sock + 1 > nfds ? p->sock + 1 : nfds;
  }
}

void relay(int peer_count, struct peer *peer_table) {
  int res, n, flag;
  struct peer *p;
  fd_set read_set;
  fd_set write_set;
  sigset_t sigmask;

  relay_init(peer_count, peer_table);

  struct timespec ts = {0, 200000000}; // 200mS
  listen_sock = peer_table->sock;      // first peer is always the return traffic target (listener)
  printf("run session(%d)\n", peer_count);
  FD_ZERO(&read_set);
  FD_ZERO(&write_set);
  running = peer_count - 1;
  while (running) {
    for (n = 0; n < peer_count; n++) {
      p = peer_table + n;
      if (p->active) {
        flag = FD_ISSET(p->sock, &read_set);
        if (readaction(p, flag))
          FD_SET(p->sock, &read_set);
        else
          FD_CLR(p->sock, &read_set);
      }
    };
    res = pselect(nfds, &read_set, &write_set, NULL, &ts, &sigmask);
    if (-1 == res) {
      printf("pselect error, errno: %d (%s)\n", errno, strerror(errno));
    } else if (0 == res) {
      // printf("pselect timeout\n");
    }
  };
  for (n = 0; n < peer_count; n++) {
    p = peer_table + n;
    close(p->sock);
    free(p->buf);
  };
  printf("relay: exit\n");
};

void setsocketnonblock(int sock) {
  fcntl(sock, F_SETFL, O_NONBLOCK);
};

void setsocketnodelay(int sock) {
  int i = 1;
  setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (void *)&i, sizeof(i));
};

void showpeer(struct peer *p) {
  printf("peer %2d: local: %s ", p->peer_index, inet_ntoa(p->local.sin_addr));
  printf("         remote: %s\n", inet_ntoa(p->remote.sin_addr));
};

void peer_report(struct peer *p) {
  printf("\npeer report\n");
  showpeer(p);
  printf("%ld/%ld/%ld bytes read/written/processed\n", p->nread, p->nwrite, p->nprocessed);
  printf("%d messages\n", *(p->msg_counts));
  printf("%d Opens\n", (p->msg_counts)[1]);
  printf("%d Updates\n", (p->msg_counts)[2]);
  printf("%d Notifications\n", (p->msg_counts)[3]);
  printf("%d Keepalives\n", (p->msg_counts)[4]);
};

void peer_reports(int peer_count, struct peer *peer_table) {
  for (int i = 0; i < peer_count; i++) {
    struct peer *p = peer_table + i;
    peer_report(p);
  }
}
