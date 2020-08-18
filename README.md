
# PLEASE NOTE THIS README IS INCOMPLETE
# KAKAPO can generate synthetic route traffic or use MRT data sources

# kakapo
BGP flood tool - source and sink
A minimalistic 'C' tool which is capable of establishing BGP peer sessions and subsequently generating or accepting very large numbers of Update messages.
All used BGP messages are sourced from preencoded binary files which allows complete flexibility in choice of payload and BGP capability options.
## BGP FSM
The implemented state machine is very basic but sufficient to bring up and maintai sessions with any type of peer.  The FSM principle is to send an initial (BGP Open) message, await any reply, and then send BGP Keepalive.

Once the FSM has completed the initial exchange the program sends the provided file which should contain a stream of BGP Update messages.  Finally, the program enters an receive loop which parses, counts and discards received messages, which should also be BGP Updates.
## Update handling and Performance
In block mode message transmission is accomplished using a single _sendfile_ system call, which ensures that the payload is sent at the fastest rate possible given the underlying hardware.
Message reception is done in two ways: the default uses libc FILE buffering which is more than fast enough for most purposes (10M updates in 0.7 seconds).  This option parses BGP messages sufficiently to count them and potentially to respond differently to specific message types or content.  A compile time option provides the capability of simply dumping all input once the initial BGP handshake is complete.  This option cannot count BGP messages: it enables a 10M Update burst to be consumed in ~0.2 seconds)
Usage
_kakapo_ supports server and client modes in the same binary.  The client mode requires an additional parameter which is the peer IP address.  If this is not provided then the program runs in server mode.  _kakapo_ requires two binary files, the first should contain a wireformat BGP Open message, the second a stream of BGP Update messages.  The invocation syntax is:
```
kakapo <open.bin> <update.bin> <peer IP address>
```
## Implementation
_kakapo_ is written in generic 'C' and uses pthreads and the glibc library.

## Generating BGP binary files
_kakapo_ has no specific requirements on message file format, but the encoding must be BGP compliant wireformat, which includes the 16 byte 0xff 'marker', followed by 16-bit message length and 8-bit message type and then payload if any.
Simple message binary files are included with the source: they and other can be generated using the Haskell language BGP toolkit hBGP (https://github.com/hdb3/hBGP), which contains source code for the tools CreateOpen, CreateUpdate and CustomUpdates.
hBGP also supports conversion utilities for MRT data sources (table dumps and update streams).
