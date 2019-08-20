

// sockbuf.c

#include <stdio.h>
#include <sys/socket.h>
//#include <sys/time.h>
#include <assert.h>
#include <errno.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "sockbuf.h"
#include "tcpflags.h"
#include "util.h"

void setsocktimeout(int sock, int timeout) {
  struct timeval tv;
  // tv.tv_sec = 20;
  tv.tv_sec = timeout;
  tv.tv_usec = 0;
  setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, (const char *)&tv, sizeof tv);
};

void bufferInit(struct sockbuf *sb, int sock, int size, int timeout) {
  int one = 1;
  setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (void *)&one, sizeof(one));
  setsockopt(sock, IPPROTO_TCP, TCP_QUICKACK, (void *)&one, sizeof(one));
  setsocktimeout(sock, timeout);
  sb->sock = sock;
  sb->timeout = timeout;
  sb->rcvtimestamp = (struct timespec){0, 0};
  sb->rcvcount = 0;
  sb->start = 0;
  sb->count = 0;
  sb->usecount = 0;
  sb->top = size;
  sb->threshold = size * 3 / 4;
  sb->base = malloc(size);
};

char *bufferedRead(struct sockbuf *sb, int rc) {

  int sockRead;

  // first recv from socket until we can satisfy the request
  // this may require a shuffle operation before the first read

  // this assertion prevents requests which cannot be met because the entire
  // buffer is smaller than the request
  assert(0 == sb->usecount++);
  assert(rc < sb->top);

  // can reset the buffer cursor for free when there is no data in it
  if (0 == sb->count)
    sb->start = 0;

  if ((rc > sb->count) && (sb->start + sb->count > sb->threshold)) {
    // reshuffle
    // //fprintf(stderr,"sockbuf.c: reshuffle\n");
    memmove(sb->base, sb->base + sb->start, sb->count);
    sb->start = 0;
  }
  while (rc > sb->count) {
    int request = sb->top - sb->start - sb->count;
    //sockRead = recv(sb->sock, sb->base + sb->start + sb->count, request, 0);
    FLAGS(sb->sock, __FILE__, __LINE__);
    sockRead = read(sb->sock, sb->base + sb->start + sb->count, request);
    FLAGS(sb->sock, __FILE__, __LINE__);
    // if zero or worse, die...
    if (sockRead < 0) {
      if (errno == EAGAIN)
        continue;
      else {
        perror(0);
        return (char *)-1;
      }
    } else if (sockRead == 0)
      return (char *)-1;
    else {
      gettime(&sb->rcvtimestamp);
      sb->count = sb->count + sockRead;
    }
  }
  // if we get here then we know there is enough in the buffer to satisfy the
  // request
  sb->count = sb->count - rc;
  int tmp = sb->start;
  sb->start = sb->start + rc;
  assert(1 == sb->usecount--);
  return (sb->base + tmp);
}

int bgp_peek(struct sockbuf *sb) {

  // look forward in buffer to check if a complete BGP message read would succeed

  // this assertion prevents requests which cannot be met because the entire
  // buffer is smaller than the request
  assert(0 == sb->usecount++);

  if (19 > sb->count) {
    assert(1 == sb->usecount--);
    return 0;
  };

  uint8_t msg_length_hi = *(sb->base + sb->start + 16) << 8;
  uint8_t msg_length_lo = *(sb->base + sb->start + 17);
  uint16_t _msg_length = msg_length_hi + msg_length_lo;
  uint16_t msg_length = (*(sb->base + sb->start + 16) << 8) + (*(sb->base + sb->start + 17));
  if (_msg_length != msg_length)
    printf("_msg_length != msg_length: %d %d (%d:%d(\n", _msg_length, msg_length, msg_length_hi, msg_length_lo);
  assert(_msg_length == msg_length);
  assert(1 == sb->usecount--);
  return (msg_length < sb->count ? 0 : 1);
}
